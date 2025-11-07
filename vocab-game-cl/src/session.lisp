(in-package #:vocab-game)

;;;; Session Management
;;;; Handles game session state tracking

(defparameter *active-sessions* (make-hash-table :test 'equal)
  "Hash table of active game sessions keyed by session-id")

(defun create-session (session-id course-id item-count)
  "Create a new game session and store it in active sessions"
  (let ((session (make-game-session
                  :session-id session-id
                  :grade course-id
                  :activities '()
                  :current-index 0
                  :score 0
                  :total-activities item-count
                  :started-at (local-time:now)
                  :user-id nil)))
    (setf (gethash session-id *active-sessions*) session)
    session))

(defun get-session (session-id)
  "Retrieve a game session by session-id"
  (gethash session-id *active-sessions*))

(defun update-session (session-id &key activity score increment-index)
  "Update session state"
  (let ((session (get-session session-id)))
    (when session
      (when activity
        (push activity (game-session-activities session)))
      (when score
        (setf (game-session-score session) score))
      (when increment-index
        (incf (game-session-current-index session)))
      session)))

(defun remove-session (session-id)
  "Remove a session from active sessions"
  (remhash session-id *active-sessions*))

(defun session-progress (session)
  "Calculate current progress percentage for a session"
  (if (zerop (game-session-total-activities session))
      0
      (floor (* 100 (game-session-current-index session))
             (game-session-total-activities session))))

(defun session-completed-p (session)
  "Check if session has completed all activities"
  (>= (game-session-current-index session)
      (game-session-total-activities session)))
