(in-package #:vocab-game)

;;;; Application Entry Point

(defparameter *app* (make-instance 'caveman2:<app>)
  "The Caveman2 application instance")

(defparameter *handler* nil
  "The running web server handler")

(defun start-server (&key (port *server-port*))
  "Start the web server"
  (setf *handler*
        (clack:clackup *app*
                      :port port
                      :debug *debug-mode*
                      :server :hunchentoot))
  (log:info "Server started on port ~a" port)
  (log:info "Visit http://localhost:~a" port)
  *handler*)

(defun stop-server ()
  "Stop the web server"
  (when *handler*
    (clack:stop *handler*)
    (setf *handler* nil)
    (log:info "Server stopped")))

(defun reload-app ()
  "Reload the application (hot reload during development)"
  (when *handler*
    (stop-server)
    (asdf:load-system :vocab-game :force t)
    (start-server)))

;; Configure Djula template directory
(djula:add-template-directory
 (asdf:system-relative-pathname :vocab-game "templates/"))

;; Helper function for rendering templates
(defun render (template-path data)
  "Render a Djula template with data"
  (let ((template (djula:compile-template* (namestring template-path))))
    (djula:render-template* template nil data)))
