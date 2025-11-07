;;;; web-server.lisp
;;;; Web server and HTTP handlers

(in-package #:vocab-game-cl)

;;; HTML Layout

(defun render-page (title body-content &key (include-game-js t))
  "Render a complete HTML page with standard layout"
  (with-html-output-to-string (s)
    (htm
     (:html
      (:head
       (:meta :charset "utf-8")
       (:meta :name "viewport" :content "width=device-width, initial-scale=1.0")
       (:title (str title))
       (:link :rel "stylesheet" :href "/static/css/style.css")
       (when include-game-js
         (htm (:script :src "/static/js/game.js"))))

      (:body
       (:div :id "app"
             (:div :id "content"
                   (str body-content)))

       ;; Loading overlay
       (:div :id "loading-overlay" :class "loading-overlay hidden"
             (:div :class "loading-spinner")
             (:p "Loading..."))

       ;; Feedback overlay
       (:div :id "feedback-overlay" :class "feedback-overlay hidden"
             (:div :class "feedback-content"
                   (:div :id "feedback-icon")
                   (:h2 :id "feedback-message")
                   (:p :id "feedback-detail"))))))))

;;; Page Handlers

(define-easy-handler (index-handler :uri "/") ()
  (setf (content-type*) "text/html; charset=utf-8")
  (start-session)
  (render-page "Playcademy Vocabulary"
               (with-html-output-to-string (s)
                 (htm
                  (:div :class "main-menu"
                        (:div :class "logo-section"
                              (:h1 :class "app-title" "Playcademy")
                              (:h2 :class "app-subtitle" "Vocabulary Learning"))

                        (:div :class "menu-content"
                              (:p :class "welcome-text"
                                  "Build your vocabulary skills with fun activities!")

                              (:div :class "grade-selection"
                                    (:h3 "Select Your Grade")
                                    (loop for grade in *grades*
                                          do (htm
                                              (:button :type "button"
                                                       :class "btn btn-grade"
                                                       :onclick (format nil "startSession(~A)" grade)
                                                       (str (format nil "Grade ~A" grade))))))

                              (:div :class "menu-actions"
                                    (:button :type "button"
                                             :class "btn btn-secondary"
                                             :onclick "viewProgress()"
                                             "View Progress"))))))))

(define-easy-handler (game-handler :uri "/game") ()
  (setf (content-type*) "text/html; charset=utf-8")
  (start-session)

  (unless (is-session-active)
    (redirect "/"))

  (render-page "Play - Playcademy Vocabulary"
               (with-html-output-to-string (s)
                 (htm
                  (:div :class "game-session"
                        (:div :class "game-header"
                              (:div :class "progress-bar-container"
                                    (:div :id "progress-bar" :class "progress-bar"))
                              (:div :id "activity-counter" :class "activity-counter"
                                    "Activity 1 of 10"))

                        (:div :id "activity-container" :class "activity-container"
                              (:p "Loading activity..."))

                        (:div :class "game-footer"
                              (:button :type "button"
                                       :class "btn btn-text"
                                       :onclick "if(confirm('Are you sure you want to quit?')) quitSession()"
                                       "Quit Session")))))))

(define-easy-handler (results-handler :uri "/results") ()
  (setf (content-type*) "text/html; charset=utf-8")
  (start-session)

  (let* ((game-session (get-game-session))
         (summary (session-summary game-session)))

    (unless summary
      (redirect "/"))

    (let* ((total (extract-json-value summary :|totalAttempts| 0))
           (correct (extract-json-value summary :|correctAttempts| 0))
           (accuracy (if (> total 0)
                        (* 100 (/ correct total))
                        0)))

      (render-page "Results - Playcademy Vocabulary"
                   (with-html-output-to-string (s)
                     (htm
                      (:div :class "results-screen"
                            (:div :class "results-header"
                                  (:h1 "Great Work!")
                                  (:div :class "confetti"))

                            (:div :class "results-content"
                                  (:div :class "stats-grid"
                                        (:div :class "stat-card"
                                              (:div :class "stat-value"
                                                    (str (format nil "~,1F%" accuracy)))
                                              (:div :class "stat-label" "Accuracy"))

                                        (:div :class "stat-card"
                                              (:div :class "stat-value"
                                                    (str (format nil "~A/~A" correct total)))
                                              (:div :class "stat-label" "Correct Answers"))

                                        (:div :class "stat-card"
                                              (:div :class "stat-value"
                                                    (str total))
                                              (:div :class "stat-label" "Questions")))

                                  (:div :class "results-message"
                                        (htm
                                         (cond
                                           ((>= accuracy 90)
                                            (htm (:p "Excellent! You're mastering these words!")))
                                           ((>= accuracy 70)
                                            (htm (:p "Good job! Keep practicing!")))
                                           (t
                                            (htm (:p "Nice effort! Practice makes perfect!")))))))

                            (:div :class "results-actions"
                                  (:button :type "button"
                                           :class "btn btn-primary"
                                           :onclick "location.href='/'"
                                           "Continue Learning")
                                  (:button :type "button"
                                           :class "btn btn-secondary"
                                           :onclick "viewProgress()"
                                           "View All Progress")))))
                   :include-game-js t))))

(define-easy-handler (progress-handler :uri "/progress") ()
  (setf (content-type*) "text/html; charset=utf-8")
  (start-session)

  (multiple-value-bind (progress error)
      (get-progress)

    (if error
        (render-page "Progress - Playcademy Vocabulary"
                     (with-html-output-to-string (s)
                       (htm
                        (:div :class "error-screen"
                              (:h2 "Unable to load progress")
                              (:p (str (extract-json-value error :|message| "Unknown error")))
                              (:button :type "button"
                                       :class "btn btn-primary"
                                       :onclick "location.href='/'"
                                       "Back to Menu")))))

        (let* ((words-learned (extract-json-value progress :|words_learned| 0))
               (accuracy (extract-json-value progress :|accuracy| 0))
               (total-attempts (extract-json-value progress :|total_attempts| 0))
               (sessions (extract-json-value progress :|sessions_completed| 0)))

          (render-page "Progress - Playcademy Vocabulary"
                       (with-html-output-to-string (s)
                         (htm
                          (:div :class "progress-screen"
                                (:div :class "progress-header"
                                      (:h1 "Your Progress"))

                                (:div :class "progress-content"
                                      (:div :class "stats-grid"
                                            (:div :class "stat-card large"
                                                  (:div :class "stat-value"
                                                        (str words-learned))
                                                  (:div :class "stat-label" "Words Learned"))

                                            (:div :class "stat-card large"
                                                  (:div :class "stat-value"
                                                        (str (format nil "~,1F%" accuracy)))
                                                  (:div :class "stat-label" "Overall Accuracy"))

                                            (:div :class "stat-card"
                                                  (:div :class "stat-value"
                                                        (str sessions))
                                                  (:div :class "stat-label" "Sessions Completed"))

                                            (:div :class "stat-card"
                                                  (:div :class "stat-value"
                                                        (str total-attempts))
                                                  (:div :class "stat-label" "Total Attempts"))))

                                (:div :class "progress-actions"
                                      (:button :type "button"
                                               :class "btn btn-primary"
                                               :onclick "location.href='/'"
                                               "Start Learning"))))))))))

;;; API Endpoints

(define-easy-handler (api-auth-handler :uri "/api/auth") ()
  (setf (content-type*) "application/json")
  (start-session)

  (multiple-value-bind (response error)
      (api-authenticate *default-user-email* *default-user-password*)

    (if error
        (json-response error)
        (json-response `(:|success| t
                        :|userId| ,(user-id *api-client*))))))

(define-easy-handler (api-courses-handler :uri "/api/courses") ()
  (setf (content-type*) "application/json")
  (start-session)

  (multiple-value-bind (response error)
      (api-get-courses)

    (if error
        (json-response error)
        (json-response response))))

(define-easy-handler (api-start-session-handler :uri "/api/session/start"
                                                :default-request-type :post) ()
  (setf (content-type*) "application/json")
  (start-session)

  (let* ((body (hunchentoot:raw-post-data :force-text t))
         (data (when (and body (> (length body) 0))
                (jonathan:parse body :as :alist)))
         (course-id (extract-json-value data :|courseId|)))

    (multiple-value-bind (response error)
        (start-new-session course-id)

      (if error
          (json-response error)
          (json-response response)))))

(define-easy-handler (api-next-activity-handler :uri "/api/activity/next") ()
  (setf (content-type*) "application/json")
  (start-session)

  (multiple-value-bind (response error)
      (load-next-activity)

    (if error
        (json-response error)
        (json-response response))))

(define-easy-handler (api-submit-answer-handler :uri "/api/activity/submit"
                                                :default-request-type :post) ()
  (setf (content-type*) "application/json")
  (start-session)

  (let* ((body (hunchentoot:raw-post-data :force-text t))
         (data (when (and body (> (length body) 0))
                (jonathan:parse body :as :alist)))
         (answer (extract-json-value data :|answer|)))

    (multiple-value-bind (response error)
        (submit-answer answer)

      (if error
          (json-response error)
          (json-response response)))))

(define-easy-handler (api-end-session-handler :uri "/api/session/end"
                                              :default-request-type :post) ()
  (setf (content-type*) "application/json")
  (start-session)

  (multiple-value-bind (response error)
      (end-session)

    (if error
        (json-response error)
        (progn
          (clear-game-session)
          (json-response response)))))

;;; Static file handler
(defun setup-static-handlers ()
  "Set up handlers for static files"
  (push (hunchentoot:create-folder-dispatcher-and-handler
         "/static/" (merge-pathnames "static/"
                                    (asdf:system-source-directory :vocab-game-cl)))
        hunchentoot:*dispatch-table*))
