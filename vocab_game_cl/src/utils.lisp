;;;; utils.lisp
;;;; Utility functions for the vocabulary game

(in-package #:vocab-game-cl)

(defun get-timestamp ()
  "Get current timestamp in milliseconds"
  (* (local-time:timestamp-to-unix (local-time:now)) 1000))

(defun make-error-response (code message)
  "Create a standardized error response"
  (jonathan:to-json
   `(:|error| (:|code| . ,code)
              (:|message| . ,message))))

(defun log-info (message &rest args)
  "Log an informational message"
  (apply #'format t (concatenate 'string "INFO: " message "~%") args))

(defun log-error (message &rest args)
  "Log an error message"
  (apply #'format t (concatenate 'string "ERROR: " message "~%") args))

(defun log-debug (message &rest args)
  "Log a debug message"
  (apply #'format t (concatenate 'string "DEBUG: " message "~%") args))

(defun string-equal-ignore-case (str1 str2)
  "Compare two strings case-insensitively"
  (and str1 str2
       (string-equal (string-trim '(#\Space #\Tab #\Newline) str1)
                     (string-trim '(#\Space #\Tab #\Newline) str2))))

(defun sanitize-string (str)
  "Sanitize a string for HTML output"
  (if str
      (cl-ppcre:regex-replace-all "[<>\"']" str
                                   (lambda (match)
                                     (case (char match 0)
                                       (#\< "&lt;")
                                       (#\> "&gt;")
                                       (#\" "&quot;")
                                       (#\' "&#39;"))))
      ""))

(defun generate-session-id ()
  "Generate a unique session ID"
  (format nil "cl-session-~A-~A"
          (get-timestamp)
          (random 1000000)))

(defun json-response (data)
  "Convert data to JSON and return as HTTP response"
  (setf (content-type*) "application/json")
  (jonathan:to-json data))

(defun extract-json-value (json-object key &optional default)
  "Safely extract a value from a JSON object (alist)"
  (or (cdr (assoc key json-object :test #'string-equal))
      default))
