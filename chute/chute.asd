(require :asdf)
(in-package :asdf)

(defsystem
    :chute :version "0.0.1"
    :depends-on (ironclad simple-date-time cl-json)
    :components ((:module source :pathname "" :components
                          ((:file "chute")))))
