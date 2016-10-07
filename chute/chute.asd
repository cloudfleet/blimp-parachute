(require :asdf)

#+abcl
(eval-when (:load-toplevel :execute)
  (dolist (required '(:abcl-contrib
                      :quicklisp-abcl
                      :jna))
    (require required)))

(in-package :asdf)

(defsystem :chute

  :version "0.4.0"
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
                        ((:file "package")))
               (:module config :pathname ""
                        :depends-on (package)
                        :serial t :components
                        ((:file "macos")
                         (:file "config-client")
                         (:file "config-server")))
               (:module source :pathname ""
                        :depends-on (config)
                        :serial t :components
                        ((:file "util")
                         (:file "fs") (:file "btrfs") (:file "zfs")
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
                        ((:file "server")))
               (:module io.cloudfleet :pathname ""
                        :depends-on (source)
                        :serial t :components
                        ((:file "engineroom")))
               (:module osx :pathname ""
                        :depends-on (source)
                        :serial t :components
                        ((:file "macos")))
               (:module test :pathname ""
                        :depends-on (server api crypt)
                        :serial t :components
                        ((:file "test")))))

;;; I'm an evil runtime thingie: figure out how to eliminate me
(eval-when (:compile-toplevel :load-toplevel :execute)
  (let ((quicklisp-dependencies (asdf:system-relative-pathname :chute "quicklisp-setup.lisp")))
    (format t "~&Doing a QL:QUICKLOAD across all dependencies declared in <file:~a> ...~^"
            quicklisp-dependencies)
    (load quicklisp-dependencies)
    (format t "~&DONE executing QL:QUICKLOAD forms from <file:~a>.~^"
            quicklisp-dependencies)))


