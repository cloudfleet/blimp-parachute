(require :asdf)
(in-package :asdf)

#| b0rked:  just load chute-quickload.lisp where possible
(eval-when (:compile-toplevel :execute)
  (load (asdf:system-relative-pathname (asdf:find-system :chute)
                                       "chute-quickload")))
|#

(defsystem :chute
  :version "0.0.7"
  :in-order-to ((test-op (load-op "test")))
  :perform (test-op (o c) (symbol-call :do-tests :rt))
  :depends-on (ironclad
               simple-date-time
               cl-json
               hunchentoot
               drakma
               osicat
               rt)
  :components ((:module source :pathname "" :serial t :components
                        ((:file "package")
                         (:file "config")
                         (:file "blob")
                         (:file "server")
                         (:file "chute")))))

