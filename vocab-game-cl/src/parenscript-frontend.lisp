(in-package #:vocab-game)

;;;; Parenscript Frontend
;;;; Client-side JavaScript generated from Common Lisp

(defmacro generate-js-file (filename &body body)
  "Generate JavaScript file from Parenscript code"
  `(with-open-file (stream (asdf:system-relative-pathname :vocab-game
                                                          ,(format nil "static/js/~a" filename))
                          :direction :output
                          :if-exists :supersede)
     (format stream "~a" (ps:ps ,@body))))

;;;; Main Frontend JavaScript

(defun generate-frontend-js ()
  "Generate the main frontend JavaScript file"
  (generate-js-file "generated.js"

    ;; Global variables
    (defvar current-session-id nil)
    (defvar current-item-id nil)
    (defvar start-time nil)

    ;; Utility functions
    (defun get-element-by-id (id)
      (chain document (get-element-by-id id)))

    (defun hide-element (id)
      (let ((el (get-element-by-id id)))
        (when el
          (setf (chain el style display) "none"))))

    (defun show-element (id)
      (let ((el (get-element-by-id id)))
        (when el
          (setf (chain el style display) "block"))))

    ;; Session management
    (defun start-session (course-id email)
      "Start a new game session"
      (setf start-time (chain *date (now)))
      (chain fetch "/start-session"
             (then (lambda (response)
                    (chain response (json))))
             (then (lambda (data)
                    (setf current-session-id (@ data session-id))
                    (window.location.href (+ "/game?sessionId=" current-session-id))))
             (catch (lambda (error)
                     (console.error "Failed to start session:" error)
                     (alert "Failed to start session. Please try again.")))))

    ;; Answer submission
    (defun submit-answer (answer)
      "Submit an answer for the current activity"
      (let ((latency (- (chain *date (now)) start-time)))
        (show-element "loading")
        (chain fetch "/submit-answer"
               (then (lambda (response)
                      (chain response (json))))
               (then (lambda (data)
                      (hide-element "loading")
                      (display-feedback data)
                      (when (@ data correct)
                        (set-timeout (lambda () (next-activity)) 2000))))
               (catch (lambda (error)
                       (hide-element "loading")
                       (console.error "Failed to submit answer:" error)
                       (alert "Failed to submit answer. Please try again."))))))

    ;; Feedback display
    (defun display-feedback (data)
      "Display feedback after answer submission"
      (let ((feedback-el (get-element-by-id "feedback"))
            (is-correct (@ data correct)))
        (when feedback-el
          (setf (@ feedback-el inner-h-t-m-l)
                (if is-correct
                    "<div class='correct'>✓ Correct!</div>"
                    (+ "<div class='incorrect'>✗ Incorrect. The answer is: "
                       (@ data correct-answer) "</div>")))
          (show-element "feedback"))))

    ;; Multiple choice
    (defun select-option (option)
      "Handle multiple choice selection"
      (submit-answer option))

    ;; Spelling activity
    (defun play-word-audio ()
      "Play audio for spelling activity"
      (let ((audio (get-element-by-id "word-audio")))
        (when audio
          (chain audio (play)))))

    (defun submit-spelling ()
      "Submit spelling answer"
      (let* ((input (get-element-by-id "spelling-input"))
             (answer (@ input value)))
        (when (> (@ answer length) 0)
          (submit-answer answer))))

    ;; Fill in the blank
    (defun submit-blank ()
      "Submit fill-in-the-blank answer"
      (let* ((input (get-element-by-id "blank-input"))
             (answer (@ input value)))
        (when (> (@ answer length) 0)
          (submit-answer answer))))

    ;; Navigation
    (defun continue-session ()
      "Continue to next activity"
      (next-activity))

    (defun next-activity ()
      "Load next activity"
      (window.location.href (+ "/game?sessionId=" current-session-id)))

    ;; Event listeners
    (chain window
           (add-event-listener "DOMContentLoaded"
                              (lambda ()
                                (console.log "Vocabulary Game Frontend Loaded"))))))

;; Generate the JavaScript file when this file is loaded
(generate-frontend-js)
