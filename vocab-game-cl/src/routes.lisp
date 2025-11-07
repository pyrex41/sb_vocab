(in-package #:vocab-game)

;;;; Web Routes
;;;; Caveman2 routes for the vocabulary game

(defroute "/" ()
  "Main menu - course/grade selection"
  (render #P"index.html"
          '(:title "Playcademy Vocabulary Game"
            :courses (list
                      (list :id "grade-3" :name "Grade 3" :grade 3)
                      (list :id "grade-4" :name "Grade 4" :grade 4)
                      (list :id "grade-5" :name "Grade 5" :grade 5)))))

(defroute ("/start-session" :method :POST) (&key |courseId| |email|)
  "Start a new game session"
  (handler-case
      (progn
        ;; Authenticate user (mock login for development)
        (when |email|
          (mock-login |email|))

        ;; Start session via API
        (multiple-value-bind (session-id item-count duration)
            (start-session |courseId|)

          ;; Create local session tracking
          (create-session session-id |courseId| item-count)

          ;; Return JSON response
          (format nil "{\"sessionId\": \"~a\", \"itemCount\": ~a, \"duration\": ~a}"
                  session-id item-count duration)))
    (error (e)
      (setf (response-status *response*) 500)
      (format nil "{\"error\": \"~a\"}" e))))

(defroute ("/game" :method :GET) (&key |sessionId|)
  "Display current activity"
  (handler-case
      (let* ((session (get-session |sessionId|))
             (activity-data (get-next-activity |sessionId|)))
        (unless session
          (error "Session not found"))

        ;; Update session with new activity
        (update-session |sessionId|
                       :activity activity-data
                       :increment-index t)

        ;; Render activity template based on type
        (render-activity activity-data session))
    (error (e)
      (render #P"error.html"
              (list :message (format nil "~a" e))))))

(defroute ("/submit-answer" :method :POST) (&key |sessionId| |itemId| |answer| |latencyMs| |timeSpentS|)
  "Submit an answer attempt"
  (handler-case
      (let* ((result (submit-attempt |sessionId| |itemId| |answer|
                                    :latency-ms (parse-integer (or |latencyMs| "0"))
                                    :time-spent-s (when |timeSpentS|
                                                   (parse-integer |timeSpentS|))))
             (session (get-session |sessionId|)))

        ;; Update score
        (when (attempt-result-correct result)
          (update-session |sessionId|
                         :score (1+ (game-session-score session))))

        ;; Return JSON response
        (format nil "{\"correct\": ~a, \"feedback\": \"~a\", \"correctAnswer\": \"~a\"}"
                (if (attempt-result-correct result) "true" "false")
                (or (attempt-result-feedback result) "")
                (or (attempt-result-correct-answer result) "")))
    (error (e)
      (setf (response-status *response*) 500)
      (format nil "{\"error\": \"~a\"}" e))))

(defroute ("/results" :method :GET) (&key |sessionId|)
  "Display session results"
  (handler-case
      (let* ((summary (finalize-session |sessionId|))
             (session (get-session |sessionId|)))

        ;; Clean up session
        (remove-session |sessionId|)

        ;; Render results
        (render #P"results.html"
                (list :total (session-summary-total-activities summary)
                      :correct (session-summary-correct-count summary)
                      :accuracy (session-summary-accuracy summary)
                      :words (session-summary-words-practiced summary)
                      :duration (session-summary-session-duration summary))))
    (error (e)
      (render #P"error.html"
              (list :message (format nil "~a" e))))))

(defroute ("/progress" :method :GET) (&key |courseId|)
  "Display user progress"
  (handler-case
      (let ((progress (get-progress |courseId|)))
        (render #P"progress.html"
                (list :total-words (progress-data-total-words-practiced progress)
                      :accuracy (or (progress-data-overall-accuracy progress) 0)
                      :sessions (or (progress-data-sessions-completed progress) 0)
                      :mastery (progress-data-mastery-levels progress))))
    (error (e)
      (render #P"error.html"
              (list :message (format nil "~a" e))))))

;;;; Helper Functions

(defun render-activity (activity session)
  "Render appropriate template based on activity type"
  (let ((template-map '((:flashcard . "activities/flashcard.html")
                        (:multiple_choice . "activities/multiple-choice.html")
                        (:spell_typed . "activities/spelling.html")
                        (:cloze_typed . "activities/fill-blank.html")
                        (:synonym-antonym . "activities/synonym-antonym.html"))))
    (let ((template-file (cdr (assoc (activity-type activity) template-map))))
      (render (pathname template-file)
              (list :activity activity
                    :session session
                    :progress (session-progress session))))))
