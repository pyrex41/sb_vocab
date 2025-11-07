(in-package #:vocab-game)

(defparameter *api-base-url*
  (or (uiop:getenv "PLAYCADEMY_API_URL")
      "https://api.playcademy.net")
  "Playcademy backend API base URL")

(defparameter *api-key*
  (or (uiop:getenv "PLAYCADEMY_API_KEY")
      "your-api-key-here")
  "API authentication key")

(defparameter *server-port*
  (parse-integer (or (uiop:getenv "PORT") "5000"))
  "Web server port")

(defparameter *debug-mode*
  (string= (uiop:getenv "DEBUG") "true")
  "Enable debug logging and hot reload")

(defparameter *session-secret*
  (or (uiop:getenv "SESSION_SECRET")
      "change-me-in-production-please")
  "Secret key for session encryption")
