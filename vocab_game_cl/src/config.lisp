;;;; config.lisp
;;;; Configuration settings for the vocabulary game

(in-package #:vocab-game-cl)

;;; API Configuration
(defparameter *api-base-url* "http://localhost:8788/api"
  "Base URL for the Playcademy backend API")

(defparameter *default-user-email* "student.fresh@demo.playcademy.com"
  "Default user email for authentication")

(defparameter *default-user-password* "password"
  "Default user password for authentication")

;;; Web Server Configuration
(defparameter *web-server-port* 8080
  "Port for the web server")

(defparameter *web-server-address* "0.0.0.0"
  "Address to bind the web server to")

;;; Game Configuration
(defparameter *session-timeout* 3600
  "Session timeout in seconds (1 hour)")

(defparameter *activity-types*
  '(:flashcard :multiple-choice :spelling :fill-blank :synonym-antonym)
  "Supported activity types")

(defparameter *feedback-delay* 1.5
  "Delay in seconds before auto-advancing after feedback")

;;; UI Configuration
(defparameter *brand-colors*
  '(:primary "#4F46E5"
    :success "#10B981"
    :error "#EF4444"
    :background "#F9FAFB"
    :text "#1F2937")
  "Playcademy brand colors")

(defparameter *grades* '(3 4 5)
  "Available grade levels")

;;; Global state
(defvar *acceptor* nil
  "HTTP acceptor instance")

(defvar *session-cookie* nil
  "Session cookie for API authentication")
