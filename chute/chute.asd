(require :asdf)

#+abcl
(eval-when (:load-toplevel :execute)
  (dolist (required '(:abcl-contrib
                      :quicklisp-abcl
                      :jna))
    (require required)))

(in-package :asdf)

(defsystem :chute
  :version "0.3.0.0"
  :perform (test-op (o c) (symbol-call :rt :do-tests))
  :depends-on (ironclad
               lparallel
               cl-date-time-parser
               simple-date-time
               cl-json
               hunchentoot
               restas
               cl-who
               
               drakma

               #| Unused: problems under linux/sbcl and solaris/*
               #-(or solaris)
               osicat
               |#

               rt)
  :components ((:module package :pathname ""
                        :serial t :components
                        ((:file "package")
                         (:file "config-client")
                         (:file "engineroom")))
               (:module source :pathname ""
                        :depends-on (package)
                        :serial t :components
                        ((:file "util")
                         (:file "btrfs") (:file "zfs")
                         (:file "note")
                         (:file "blob")
                         (:file "chute")
                         (:file "client")
                         (:file "transfer-http")))
               (:module crypt :pathname ""
                        :depends-on (source)
                        :serial t :components
                        ((:file "crypt")))
               (:module api :pathname ""
                        :depends-on (source)
                        :serial t :components
                        ((:file "api-server")
                         (:file "api")))
               (:module server :pathname ""
                        :depends-on (source)
                        :serial t :components
                        ((:file "config-server")
                         (:file "server")))
               (:module osx :pathname ""
                        :depends-on (source)
                        :serial t :components
                        ((:file "osx")))
               (:module test :pathname ""
                        :depends-on (server api crypt)
                        :serial t :components
                        ((:file "test")))))


