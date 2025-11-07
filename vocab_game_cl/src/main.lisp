;;;; main.lisp
;;;; Main entry point for the vocabulary game server

(in-package #:vocab-game-cl)

(defun start-server (&key (port *web-server-port*) (address *web-server-address*))
  "Start the web server"
  (when *acceptor*
    (log-info "Server already running")
    (return-from start-server *acceptor*))

  (log-info "Starting Playcademy Vocabulary Game Server...")
  (log-info "Port: ~A" port)
  (log-info "Address: ~A" address)

  ;; Set up static file handlers
  (setup-static-handlers)

  ;; Create and start the acceptor
  (setf *acceptor* (make-instance 'easy-acceptor
                                  :port port
                                  :address address))

  (start *acceptor*)

  ;; Authenticate with the backend
  (log-info "Authenticating with backend...")
  (multiple-value-bind (response error)
      (api-authenticate *default-user-email* *default-user-password*)
    (if error
        (log-error "Failed to authenticate: ~A" (extract-json-value error :|message|))
        (log-info "Authenticated as user: ~A" (user-id *api-client*))))

  (log-info "Server started successfully!")
  (log-info "Visit http://localhost:~A to play" port)

  *acceptor*)

(defun stop-server ()
  "Stop the web server"
  (when *acceptor*
    (log-info "Stopping server...")
    (stop *acceptor*)
    (setf *acceptor* nil)
    (log-info "Server stopped")))

(defun restart-server ()
  "Restart the web server"
  (stop-server)
  (sleep 0.5)
  (start-server))

(defun main ()
  "Main entry point"
  (handler-case
      (progn
        (format t "~%")
        (format t "╔═══════════════════════════════════════════╗~%")
        (format t "║   Playcademy Vocabulary Learning Game    ║~%")
        (format t "║        Common Lisp Implementation         ║~%")
        (format t "╚═══════════════════════════════════════════╝~%")
        (format t "~%")

        (start-server)

        (format t "~%Press Ctrl+C to stop the server~%~%")

        ;; Keep the server running
        (loop (sleep 1)))

    ;; Handle Ctrl+C gracefully
    (#+sbcl sb-sys:interactive-interrupt
     #+ccl  ccl:interrupt-signal-condition
     #+clisp system::simple-interrupt-condition
     #+ecl ext:interactive-interrupt
     #+allegro excl:interrupt-signal
     ()
     (format t "~%~%Shutting down gracefully...~%")
     (stop-server)
     (format t "Goodbye!~%")
     #+sbcl (sb-ext:exit :code 0)
     #+ccl (ccl:quit)
     #+clisp (ext:quit)
     #+ecl (ext:quit)
     #+allegro (excl:exit))))
