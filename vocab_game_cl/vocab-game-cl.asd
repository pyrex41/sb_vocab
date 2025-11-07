;;;; vocab-game-cl.asd
;;;; ASDF system definition for the Playcademy Vocabulary Game (Common Lisp implementation)

(defsystem "vocab-game-cl"
  :version "1.0.0"
  :author "Playcademy"
  :description "A vocabulary learning game for elementary students (grades 3-5)"
  :depends-on (#:hunchentoot
               #:cl-who
               #:parenscript
               #:dexador
               #:jonathan
               #:alexandria
               #:cl-ppcre
               #:local-time
               #:bordeaux-threads)
  :components ((:module "src"
                :components
                ((:file "package")
                 (:file "config" :depends-on ("package"))
                 (:file "utils" :depends-on ("package"))
                 (:file "api-client" :depends-on ("package" "config" "utils"))
                 (:file "session-manager" :depends-on ("package" "api-client" "utils"))
                 (:file "activities" :depends-on ("package" "session-manager"))
                 (:file "web-server" :depends-on ("package" "session-manager" "activities"))
                 (:file "main" :depends-on ("web-server")))))
  :in-order-to ((test-op (test-op "vocab-game-cl/tests"))))

(defsystem "vocab-game-cl/tests"
  :depends-on (#:vocab-game-cl
               #:fiveam)
  :components ((:module "tests"
                :components
                ((:file "package")
                 (:file "tests" :depends-on ("package")))))
  :perform (test-op (op c) (symbol-call :fiveam '#:run! :vocab-game-cl-tests)))
