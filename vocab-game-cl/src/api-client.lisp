(in-package #:vocab-game)

;;;; Playcademy API Client
;;;; Provides functions to interact with the Playcademy backend REST API

(defparameter *api-version* "/v1"
  "API version prefix")

(defun api-url (path)
  "Construct full API URL from path"
  (format nil "~a~a~a" *api-base-url* *api-version* path))

(defun make-headers ()
  "Create HTTP headers for API requests"
  `(("Content-Type" . "application/json")
    ("Accept" . "application/json")))

;;;; Session Management

(defun start-session (course-id &key time-budget-s)
  "Start a new vocabulary game session
   Returns: session-id, item-count, planned-duration-s"
  (handler-case
      (let* ((payload (jonathan:to-json
                       (remove nil
                               (list (cons "courseId" course-id)
                                     (when time-budget-s
                                       (cons "timeBudgetS" time-budget-s)))
                               :key #'cdr)))
             (response (dex:post (api-url "/session/start")
                                :headers (make-headers)
                                :content payload
                                :cookie-jar t)))  ; Enable cookie handling
        (let ((data (jonathan:parse response)))
          (values
           (gethash "sessionId" data)
           (gethash "itemCount" data)
           (gethash "plannedDurationS" data)
           (gethash "resuming" data))))
    (error (e)
      (log:error "Failed to start session: ~a" e)
      (error "Could not start session. Please check your connection."))))

(defun get-next-activity (session-id)
  "Get the next activity for the current session
   Returns: activity struct with type, word, and params"
  (handler-case
      (let* ((response (dex:post (api-url (format nil "/session/~a/next" session-id))
                                 :headers (make-headers)
                                 :cookie-jar t))
             (data (jonathan:parse response)))
        (parse-activity-response data))
    (error (e)
      (log:error "Failed to get next activity: ~a" e)
      (error "Could not load next activity."))))

(defun submit-attempt (session-id item-id answer &key latency-ms hints-used retries-used time-spent-s)
  "Submit an answer attempt for an activity
   Returns: attempt-result struct with correct, score, feedback"
  (handler-case
      (let* ((attempt-id (format nil "~a" (uuid:make-v4-uuid)))
             (payload (jonathan:to-json
                       (remove nil
                               (list (cons "itemId" item-id)
                                     (cons "answer" answer)
                                     (cons "attemptId" attempt-id)
                                     (cons "latencyMs" (or latency-ms 0))
                                     (cons "hintsUsed" (or hints-used 0))
                                     (cons "retriesUsed" (or retries-used 0))
                                     (when time-spent-s
                                       (cons "timeSpentS" time-spent-s)))
                               :key #'cdr)))
             (response (dex:post (api-url (format nil "/session/~a/attempt" session-id))
                                :headers (make-headers)
                                :content payload
                                :cookie-jar t))
             (data (jonathan:parse response)))
        (make-attempt-result
         :correct (gethash "correct" data)
         :feedback (gethash "feedback" data)
         :correct-answer (gethash "correctAnswer" data)))
    (error (e)
      (log:error "Failed to submit attempt: ~a" e)
      (error "Could not submit answer."))))

(defun finalize-session (session-id)
  "Finalize the current session and get summary
   Returns: session-summary struct"
  (handler-case
      (let* ((response (dex:post (api-url (format nil "/session/~a/finalize" session-id))
                                 :headers (make-headers)
                                :cookie-jar t))
             (data (jonathan:parse response))
             (summary (gethash "summary" data)))
        (make-session-summary
         :total-activities (gethash "itemsAnswered" data)
         :correct-count (gethash "correctCount" summary)
         :accuracy (gethash "accuracy" data)
         :words-practiced (gethash "wordsPracticed" summary)
         :session-duration (gethash "durationS" summary)))
    (error (e)
      (log:error "Failed to finalize session: ~a" e)
      (error "Could not end session."))))

;;;; Progress Tracking

(defun get-progress (course-id)
  "Get user progress for a course
   Returns: progress-data struct"
  (handler-case
      (let* ((response (dex:get (api-url (format nil "/me/progress/course/~a" course-id))
                               :headers (make-headers)
                               :cookie-jar t))
             (data (jonathan:parse response))
             (progress (gethash "progress" data)))
        (make-progress-data
         :total-words-practiced (+ (gethash "learning" progress 0)
                                   (gethash "reviewing" progress 0)
                                   (gethash "mastered" progress 0))
         :overall-accuracy nil  ; Not provided by this endpoint
         :sessions-completed nil  ; Not provided by this endpoint
         :mastery-levels (let ((table (make-hash-table :test 'equal)))
                          (setf (gethash "new" table) (gethash "new" progress 0))
                          (setf (gethash "learning" table) (gethash "learning" progress 0))
                          (setf (gethash "reviewing" table) (gethash "reviewing" progress 0))
                          (setf (gethash "mastered" table) (gethash "mastered" progress 0))
                          table)))
    (error (e)
      (log:error "Failed to get progress: ~a" e)
      nil)))  ; Return nil on error for progress

;;;; Authentication

(defun mock-login (email)
  "Authenticate user with mock login for development
   Sets authentication cookie for subsequent requests"
  (handler-case
      (let* ((payload (jonathan:to-json (list (cons "email" email))))
             (response (dex:post (api-url "/auth/mock-login")
                                :headers (make-headers)
                                :content payload
                                :cookie-jar t))
             (data (jonathan:parse response)))
        (gethash "user" data))
    (error (e)
      (log:error "Failed to login: ~a" e)
      (error "Login failed."))))

;;;; Helper Functions

(defun parse-activity-response (data)
  "Parse activity data from API response into activity struct"
  (let ((activity-type (intern (string-upcase (gethash "activityType" data)) :keyword))
        (word-data (parse-word-data (gethash "word" data)))
        (params (gethash "params" data)))
    (make-activity
     :type activity-type
     :word word-data
     :options (when params
                (coerce (gethash "options" params) 'list)))))

(defun parse-word-data (word-hash)
  "Parse word data from API response into word-data struct"
  (make-word-data
   :id (gethash "wordId" word-hash)
   :word (gethash "headword" word-hash)
   :definition (gethash "definition" word-hash)
   :example (gethash "example" word-hash)
   :synonyms (when (gethash "synonyms" word-hash)
              (coerce (gethash "synonyms" word-hash) 'list))
   :antonyms (when (gethash "antonyms" word-hash)
              (coerce (gethash "antonyms" word-hash) 'list))
   :audio-url (gethash "audioUrl" word-hash)))
