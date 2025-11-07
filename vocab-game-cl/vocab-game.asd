(defsystem "vocab-game"
  :description "Playcademy Vocabulary Learning Game - Common Lisp Frontend"
  :version "1.0.0"
  :author "Playcademy Team <team@playcademy.net>"
  :license "MIT"
  :depends-on (#:caveman2          ; Web framework
               #:clack             ; Web server interface
               #:djula             ; Templates
               #:parenscript       ; Lisp → JavaScript
               #:dexador           ; HTTP client
               #:jonathan          ; JSON parsing
               #:cl-who            ; HTML generation (alternative)
               #:lack              ; Middleware
               #:lack-middleware-session
               #:alexandria        ; Utilities
               #:serapeum          ; More utilities
               #:cl-ppcre          ; Regular expressions
               #:local-time        ; Time handling
               #:log4cl            ; Logging
               )
  :components ((:module "src"
                :components
                ((:file "package")
                 (:file "config" :depends-on ("package"))
                 (:file "models" :depends-on ("package"))
                 (:file "api-client" :depends-on ("package" "config" "models"))
                 (:file "session" :depends-on ("package" "models" "api-client"))
                 (:file "parenscript-frontend" :depends-on ("package"))
                 (:file "routes" :depends-on ("package" "session" "api-client"))
                 (:file "main" :depends-on ("package" "routes" "config")))))
  :in-order-to ((test-op (test-op "vocab-game/tests"))))

(defsystem "vocab-game/tests"
  :depends-on (#:vocab-game
               #:fiveam)
  :components ((:module "tests"
                :components
                ((:file "package")
                 (:file "api-client-tests" :depends-on ("package"))
                 (:file "session-tests" :depends-on ("package"))))))
