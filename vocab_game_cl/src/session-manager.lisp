;;;; session-manager.lisp
;;;; Session management and game flow coordination

(in-package #:vocab-game-cl)

(defclass game-session ()
  ((session-id :initarg :session-id
               :accessor session-id
               :documentation "Playcademy backend session ID")
   (course-id :initarg :course-id
              :accessor course-id
              :documentation "Course ID being studied")
   (item-count :initarg :item-count
               :accessor item-count
               :initform 0
               :documentation "Total items in session")
   (current-item-id :initarg :current-item-id
                    :accessor current-item-id
                    :initform nil
                    :documentation "Current activity item ID")
   (current-activity :initarg :current-activity
                     :accessor current-activity
                     :initform nil
                     :documentation "Current activity data")
   (activity-index :initarg :activity-index
                   :accessor activity-index
                   :initform 0
                   :documentation "Current activity index")
   (start-time :initarg :start-time
               :accessor start-time
               :initform (get-timestamp)
               :documentation "Session start timestamp")
   (activity-start-time :initarg :activity-start-time
                        :accessor activity-start-time
                        :initform nil
                        :documentation "Current activity start timestamp")
   (completed :initarg :completed
              :accessor session-completed-p
              :initform nil
              :documentation "Whether session is completed")
   (summary :initarg :summary
            :accessor session-summary
            :initform nil
            :documentation "Session summary data"))
  (:documentation "Game session state"))

;;; Session storage (in-memory for now)
(defvar *sessions* (make-hash-table :test 'equal)
  "Hash table storing active game sessions by web session ID")

(defun get-game-session (&optional (web-session-id (session-cookie-value "hunchentoot-session")))
  "Get or create a game session for the current web session"
  (or (gethash web-session-id *sessions*)
      (setf (gethash web-session-id *sessions*)
            (make-instance 'game-session))))

(defun clear-game-session (&optional (web-session-id (session-cookie-value "hunchentoot-session")))
  "Clear the game session for the current web session"
  (remhash web-session-id *sessions*))

;;; Session operations

(defun start-new-session (course-id)
  "Start a new learning session
   Returns: (values session-data error-info)"
  (log-info "Starting new session for course: ~A" course-id)

  (multiple-value-bind (response error)
      (api-start-session course-id)

    (if error
        (progn
          (log-error "Failed to start session: ~A" error)
          (values nil error))
        (let* ((session-id (extract-json-value response :|sessionId|))
               (item-count (extract-json-value response :|itemCount| 0))
               (game-session (get-game-session)))

          (log-info "Session started: ~A (items: ~A)" session-id item-count)

          (setf (session-id game-session) session-id
                (course-id game-session) course-id
                (item-count game-session) item-count
                (activity-index game-session) 0
                (session-completed-p game-session) nil
                (start-time game-session) (get-timestamp))

          (values `(:|sessionId| ,session-id
                   :|itemCount| ,item-count
                   :|courseId| ,course-id)
                  nil)))))

(defun load-next-activity ()
  "Load the next activity in the current session
   Returns: (values activity-data error-info)"
  (let ((game-session (get-game-session)))
    (unless (session-id game-session)
      (return-from load-next-activity
        (values nil `(:|error| (:|code| . "NO_SESSION")
                              (:|message| . "No active session")))))

    (log-info "Loading next activity for session: ~A" (session-id game-session))

    (multiple-value-bind (response error)
        (api-next-activity (session-id game-session))

      (if error
          (progn
            (log-error "Failed to load next activity: ~A" error)
            (values nil error))
          (let ((completed (extract-json-value response :|completed|))
                (item-id (extract-json-value response :|itemId|))
                (activity-type (extract-json-value response :|activityType|))
                (word (extract-json-value response :|word|))
                (options (extract-json-value response :|options|)))

            (cond
              ;; Session completed
              (completed
               (log-info "Session completed")
               (setf (session-completed-p game-session) t)
               (values `(:|completed| t) nil))

              ;; Next activity
              (t
               (incf (activity-index game-session))
               (setf (current-item-id game-session) item-id
                     (current-activity game-session) response
                     (activity-start-time game-session) (get-timestamp))

               (log-info "Loaded activity ~A: ~A (item: ~A)"
                        (activity-index game-session)
                        activity-type
                        item-id)

               (values `(:|itemId| ,item-id
                        :|activityType| ,activity-type
                        :|word| ,word
                        :|options| ,options
                        :|index| ,(activity-index game-session)
                        :|total| ,(item-count game-session))
                       nil))))))))

(defun submit-answer (answer)
  "Submit an answer for the current activity
   Returns: (values result error-info)"
  (let* ((game-session (get-game-session))
         (session-id (session-id game-session))
         (item-id (current-item-id game-session))
         (latency (if (activity-start-time game-session)
                     (- (get-timestamp) (activity-start-time game-session))
                     0)))

    (unless session-id
      (return-from submit-answer
        (values nil `(:|error| (:|code| . "NO_SESSION")
                              (:|message| . "No active session")))))

    (unless item-id
      (return-from submit-answer
        (values nil `(:|error| (:|code| . "NO_ACTIVITY")
                              (:|message| . "No active activity")))))

    (log-info "Submitting answer for item ~A (latency: ~Ams)" item-id latency)

    (multiple-value-bind (response error)
        (api-submit-attempt session-id item-id answer latency)

      (if error
          (progn
            (log-error "Failed to submit answer: ~A" error)
            (values nil error))
          (let ((correct (extract-json-value response :|correct|))
                (feedback (extract-json-value response :|feedback|)))

            (log-info "Answer result: ~A" (if correct "CORRECT" "INCORRECT"))

            (values `(:|correct| ,correct
                     :|feedback| ,feedback
                     :|answer| ,answer)
                    nil))))))

(defun end-session ()
  "End the current session
   Returns: (values summary error-info)"
  (let* ((game-session (get-game-session))
         (session-id (session-id game-session)))

    (unless session-id
      (return-from end-session
        (values nil `(:|error| (:|code| . "NO_SESSION")
                              (:|message| . "No active session")))))

    (log-info "Ending session: ~A" session-id)

    (multiple-value-bind (response error)
        (api-end-session session-id)

      (if error
          (progn
            (log-error "Failed to end session: ~A" error)
            (values nil error))
          (let* ((total-attempts (extract-json-value response :|totalAttempts| 0))
                 (correct-attempts (extract-json-value response :|correctAttempts| 0))
                 (accuracy (if (> total-attempts 0)
                              (* 100 (/ correct-attempts total-attempts))
                              0))
                 (duration (- (get-timestamp) (start-time game-session))))

            (setf (session-summary game-session) response)

            (log-info "Session ended - Accuracy: ~,1F% (~A/~A)"
                     accuracy correct-attempts total-attempts)

            (values `(:|sessionId| ,session-id
                     :|totalAttempts| ,total-attempts
                     :|correctAttempts| ,correct-attempts
                     :|accuracy| ,accuracy
                     :|duration| ,duration)
                    nil))))))

(defun get-progress ()
  "Get user progress statistics
   Returns: (values progress error-info)"
  (let ((user-id (user-id *api-client*)))
    (unless user-id
      (return-from get-progress
        (values nil `(:|error| (:|code| . "NOT_AUTHENTICATED")
                              (:|message| . "User not authenticated")))))

    (log-info "Getting progress for user: ~A" user-id)

    (api-get-progress user-id)))

(defun get-current-activity ()
  "Get the current activity data"
  (current-activity (get-game-session)))

(defun is-session-active ()
  "Check if there's an active session"
  (let ((game-session (get-game-session)))
    (and (session-id game-session)
         (not (session-completed-p game-session)))))
