(require :asdf)
(in-package :asdf)

#| b0rked:  just load chute-quickload.lisp where possible
(eval-when (:compile-toplevel :execute)
  (load (asdf:system-relative-pathname (asdf:find-system :chute)
                                       #| "chute-quickload")))
|#

(defsystem
    :chute :version "0.0.5"
#| AFSF&!@# you Fare
    :in-order-to 
      ((compile-op (load-op 
|#
    :depends-on (ironclad
                 simple-date-time
                 cl-json)
    :components ((:module source :pathname "" :serial t :components
                          ((:file "package")
                           (:file "chute")))))
