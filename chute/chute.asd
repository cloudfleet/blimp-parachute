(require :asdf)
(in-package :asdf)

(eval-when (:compile-toplevel)
  (loop :for symbol :in '(ironclad simple-date-time cl-json)
     :doing (ql:quickload symbol)))


(defsystem
    :chute :version "0.0.3"
    :depends-on (ironclad
                 simple-date-time
                 cl-json)
    :components ((:module source :pathname "" :serial t :components
                          ((:file "package")
                           (:file "chute")))))
