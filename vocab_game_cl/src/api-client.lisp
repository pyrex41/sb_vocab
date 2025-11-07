;;;; api-client.lisp
;;;; HTTP API client for Playcademy backend communication

(in-package #:vocab-game-cl)

(defclass api-client ()
  ((base-url :initarg :base-url
             :initform *api-base-url*
             :accessor base-url
             :documentation "Base URL for API requests")
   (session-cookie :initarg :session-cookie
                   :initform nil
                   :accessor session-cookie
                   :documentation "Session cookie for authentication")
   (user-id :initarg :user-id
            :initform nil
            :accessor user-id
            :documentation "Authenticated user ID"))
  (:documentation "HTTP client for Playcademy backend API"))

(defvar *api-client* (make-instance 'api-client)
  "Global API client instance")

(defun make-api-url (endpoint)
  "Construct full API URL from endpoint"
  (concatenate 'string (base-url *api-client*) endpoint))

(defun extract-cookie-from-headers (headers)
  "Extract session cookie from Set-Cookie header"
  (when-let ((set-cookie (cdr (assoc :set-cookie headers))))
    (let ((cookie-parts (cl-ppcre:split ";" set-cookie)))
      (when cookie-parts
        (car cookie-parts)))))

(defun make-request (method endpoint &key body)
  "Make HTTP request to the API
   Returns: (values response-data error-info)"
  (handler-case
      (let* ((url (make-api-url endpoint))
             (headers (when (session-cookie *api-client*)
                       `(("Cookie" . ,(session-cookie *api-client*)))))
             (content (when body (jonathan:to-json body)))
             (content-type (when body "application/json")))

        (log-debug "API Request: ~A ~A" method url)
        (when body
          (log-debug "Request Body: ~A" content))

        (multiple-value-bind (response status response-headers)
            (dex:request url
                        :method method
                        :headers headers
                        :content content
                        :content-type content-type
                        :force-string t)

          ;; Extract and store session cookie if present
          (when-let ((cookie (extract-cookie-from-headers response-headers)))
            (setf (session-cookie *api-client*) cookie)
            (log-debug "Session cookie updated"))

          (log-debug "API Response Status: ~A" status)

          (cond
            ;; Success responses (2xx)
            ((and (>= status 200) (< status 300))
             (let ((parsed-response (if (and response (> (length response) 0))
                                       (jonathan:parse response :as :alist)
                                       nil)))
               (log-debug "Parsed Response: ~A" parsed-response)
               (values parsed-response nil)))

            ;; Error responses
            (t
             (log-error "API Error: Status ~A, Response: ~A" status response)
             (values nil `(:|error| (:|code| . "HTTP_ERROR")
                                    (:|message| . ,(format nil "HTTP ~A" status))
                                    (:|status| . ,status)))))))

    ;; Network errors
    (dex:http-request-failed (e)
      (log-error "HTTP Request Failed: ~A" e)
      (values nil `(:|error| (:|code| . "NETWORK_ERROR")
                             (:|message| . "Failed to connect to server"))))

    ;; Timeout errors
    (usocket:timeout-error (e)
      (log-error "Request Timeout: ~A" e)
      (values nil `(:|error| (:|code| . "TIMEOUT")
                             (:|message| . "Request timed out"))))

    ;; JSON parsing errors
    (jonathan:parse-error (e)
      (log-error "JSON Parse Error: ~A" e)
      (values nil `(:|error| (:|code| . "PARSE_ERROR")
                             (:|message| . "Failed to parse server response"))))

    ;; Catch-all for other errors
    (error (e)
      (log-error "Unexpected Error: ~A" e)
      (values nil `(:|error| (:|code| . "UNKNOWN_ERROR")
                             (:|message| . ,(format nil "~A" e)))))))

;;; API Endpoints

(defun api-authenticate (email password)
  "Authenticate user with email and password
   Returns: (values user-data error-info)"
  (multiple-value-bind (response error)
      (make-request :post "/auth/sign-in/email"
                   :body `(:|email| ,email
                          :|password| ,password))
    (if error
        (values nil error)
        (progn
          (when-let ((user-id (extract-json-value response :|id|)))
            (setf (user-id *api-client*) user-id))
          (values response nil)))))

(defun api-get-courses ()
  "Get available courses
   Returns: (values courses error-info)"
  (make-request :get "/content/course"))

(defun api-start-session (course-id)
  "Start a new learning session
   Returns: (values session-data error-info)"
  (make-request :post "/session/start"
               :body `(:|courseId| ,course-id)))

(defun api-next-activity (session-id)
  "Get next activity in the session
   Returns: (values activity-data error-info)"
  (make-request :post (format nil "/session/~A/next" session-id)
               :body `(:|dummy| "dummy")))  ; Empty body as object

(defun api-submit-attempt (session-id item-id answer latency-ms)
  "Submit an activity attempt
   Returns: (values result error-info)"
  (make-request :post (format nil "/session/~A/attempt" session-id)
               :body `(:|itemId| ,item-id
                      :|answer| ,answer
                      :|latencyMs| ,latency-ms)))

(defun api-end-session (session-id)
  "End the current session
   Returns: (values summary error-info)"
  (make-request :post "/session/end"
               :body `(:|session_id| ,session-id)))

(defun api-get-progress (user-id)
  "Get user progress statistics
   Returns: (values progress-data error-info)"
  (make-request :get (format nil "/progress?user_id=~A" user-id)))
