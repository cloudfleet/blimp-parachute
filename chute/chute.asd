(require :asdf)
(in-package :asdf)

(defsystem
    :chute :version "0.0.2"
    :depends-on (ironclad
                 simple-date-time
                 cl-json)
    :components ((:module source :pathname "" :serial t :components
                          ((:file "package")
                           (:file "chute")))))
