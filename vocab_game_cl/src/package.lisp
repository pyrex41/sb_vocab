;;;; package.lisp
;;;; Package definitions for the vocabulary game

(defpackage #:vocab-game-cl
  (:use #:cl)
  (:import-from #:alexandria
                #:hash-table-keys
                #:if-let
                #:when-let)
  (:import-from #:cl-who
                #:with-html-output-to-string
                #:with-html-output
                #:htm
                #:str
                #:esc
                #:fmt)
  (:import-from #:parenscript
                #:ps
                #:ps*
                #:@
                #:chain)
  (:import-from #:hunchentoot
                #:acceptor
                #:easy-acceptor
                #:define-easy-handler
                #:start
                #:stop
                #:session-value
                #:start-session
                #:session-cookie-value
                #:*session*
                #:parameter
                #:post-parameter
                #:header-in
                #:set-cookie
                #:cookie-in
                #:redirect
                #:content-type*
                #:return-code*)
  (:export #:start-server
           #:stop-server
           #:main))

(in-package #:vocab-game-cl)
