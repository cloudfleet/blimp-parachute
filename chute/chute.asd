(require :asdf)
(in-package :asdf)

(eval-when (:compile-toplevel :execute)
  (loop
     :for symbol 
     :in '(ironclad simple-date-time cl-json drakma caveman2)
     :doing (ql:quickload symbol)))


(defsystem
    :chute :version "0.0.4"
    :depends-on (ironclad
                 simple-date-time
                 cl-json)
    :components ((:module source :pathname "" :serial t :components
                          ((:file "package")
                           (:file "chute")))))
