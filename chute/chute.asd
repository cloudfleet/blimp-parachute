(require :asdf)
(in-package :asdf)

#| b0rked:  just load chute-quickload.lisp where possible
(eval-when (:compile-toplevel :execute)
  (load (asdf:system-relative-pathname (asdf:find-system :chute)
                                       "chute-quickload")))
|#

(defsystem :chute
  :version "0.0.8"
  :perform (test-op (o c) (symbol-call :rt :do-tests))
  :depends-on (ironclad
               simple-date-time
               cl-json
               hunchentoot
               drakma
               osicat
               rt)
  :components ((:module package :pathname ""
                        :serial t :components
                        ((:file "package")))
               (:module client :pathname ""
                        :depends-on (package)
                        :serial t :components
                        ((:file "config")
                         (:file "note")
                         (:file "blob")
                         (:file "client")
                         (:file "chute")))
               (:module server :pathname ""
                        :depends-on (package)
                        :serial t :components
                        ((:file "server")))
               (:module test :pathname ""
                        :depends-on (package client)
                        :serial t :components
                        ((:file "test")))))


