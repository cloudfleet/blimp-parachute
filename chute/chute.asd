(defsystem chute
  :version "0.5.0"
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
               osicat
               rt)
  :components ((:module package :pathname ""
                        :serial t :components
                        ((:file "package")))
	       (:module model :pathname ""
                        :depends-on (package)
                        :components
                        ((:file "model")))
               (:module config :pathname ""
                        :depends-on (model)
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
                        ((:file "macos"))))
    :in-order-to ((asdf:test-op (asdf:test-op chute/t))))

(defsystem chute/t
    :defsystem-depends-on (prove-asdf)
    :depends-on (prove
		 chute)
    :perform (asdf:test-op (op c)
			   (uiop:symbol-call :prove-asdf 'run-test-system c))
    :components ((:module test
			  :pathname "t/"
			  :components
			  ((:test-file "aes")
			   (:test-file "snapshot")
			   (:test-file "config")
			   (:test-file "test")))))



