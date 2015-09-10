(require :asdf)
(in-package :asdf)

(defsystem
    :chute :version "0.0.1"
    :depends-on (ironclad)
    :components ((:module source :pathname "" :components
                          ((:file "chute")))))
