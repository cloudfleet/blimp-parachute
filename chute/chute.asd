#+abcl
(dolist (require '(:abcl-contrib :quicklisp-abcl))
  (require require))

(require :asdf)
(in-package :asdf)

#| b0rked:  just load chute-quickload.lisp where possible
(eval-when (:compile-toplevel :execute)
  (load (asdf:system-relative-pathname (asdf:find-system :chute)
                                       "quicklisp-setup")))
|#

(defsystem :chute
  :version "0.2.0.0"
  :perform (test-op (o c) (symbol-call :rt :do-tests))
  :depends-on (ironclad
               lparallel
               simple-date-time
               cl-json
               hunchentoot
               drakma
               #-(or solaris)
               osicat
               rt)
  :components ((:module package :pathname ""
                        :serial t :components
                        ((:file "package")
                         (:file "config-client")
                         (:file "engineroom")))
               (:module crypt :pathname ""
                        :depends-on (source)
                        :serial t :components
                        ((:file "crypt")))
               (:module source :pathname ""
                        :depends-on (package)
                        :serial t :components
                        ((:file "util")
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
                        ((:file "config-server")
                         (:file "server")))
               (:module test :pathname ""
                        :depends-on (client)
                        :serial t :components
                        ((:file "test")))))


