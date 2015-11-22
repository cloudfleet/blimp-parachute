(require :asdf)
(in-package :asdf)

#| b0rked:  just load chute-quickload.lisp where possible
(eval-when (:compile-toplevel :execute)
  (load (asdf:system-relative-pathname (asdf:find-system :chute)
                                       "chute-quickload")))
|#

(defsystem :chute
  :version "0.0.11.0"
  :perform (test-op (o c) (symbol-call :rt :do-tests))
  :depends-on (ironclad
               simple-date-time
               cl-json
               hunchentoot
               drakma
               #-(or solaris)
               osicat
               rt)
  :components ((:module package :pathname ""
                        :serial t :components
                        ((:file "package")))
               (:module crypt :pathname ""
                        :depends-on (source)
                        :serial t :components
                        ((:file "crypt")))
               (:module source :pathname ""
                        :depends-on (package)
                        :serial t :components
                        ((:file "config")
                         (:file "util")
                         (:file "btrfs")
                         (:file "note")
                         (:file "blob")
                         (:file "chute")))
               (:module client :pathname ""
                        :depends-on (source)
                        :serial t :components
                        ((:file "client")))
               (:module server :pathname ""
                        :depends-on (source)
                        :serial t :components
                        ((:file "server")))
               (:module test :pathname ""
                        :depends-on (client)
                        :serial t :components
                        ((:file "test")))))


