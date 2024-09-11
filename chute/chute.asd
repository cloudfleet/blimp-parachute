(defsystem chute
  :version "0.6.3"
  :components nil
  :in-order-to ((asdf:test-op
                 (asdf:test-op chute/t))))
   
(defsystem chute/implementation
  :depends-on (ironclad
               lparallel
               cl-date-time-parser
               simple-date-time
               cl-json
               hunchentoot
               restas
               cl-who
               drakma
               osicat ;;; ??? needed
               #+abcl
               chute/uri
               rt)
  :components ((:module package
                :pathname "./"
                :components ((:file "package")))
               (:module model
                 :pathname "./"
                 :depends-on (package)
                 :components ((:file "model")))
               (:module config :pathname ""
                        :depends-on (model)
                        :serial t :components
                        ((:file "macos")
                         (:file "config-client")
                         (:file "config-server")))
               (:module source :pathname ""
                        :depends-on (model config)
                        :serial t :components
                        ((:file "util")
                         (:file "fs")
                         (:file "rsync")
                         (:file "btrfs")
                         (:file "zfs")
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
                        ((:file "macos")))))

#+abcl
(progn 
  (defsystem chute/rdf
    :defsystem-depends-on (abcl-asdf)
    :depends-on (jeannie))

  (defsystem chute/rdf/t
    :defsystem-depends-on (prove-asdf)
    :components ((:module test
		  :pathname "t/"
		  :components
                  ((:test-file "rdf")))))
  (defsystem chute/uri
    :components ((:module abcl
                  :pathname "./"
                  :components
                  ((:file "uri"))))))
#-abcl "Needs the Bear for these things (bootstrapping)."

(defsystem chute/t
    :defsystem-depends-on (prove-asdf)
  :depends-on (prove
	       chute/implementation)
    :perform (asdf:test-op (op c)
		           (uiop:symbol-call :prove-asdf 'run-test-system c)
                           (when (eq uiop/os:*implementation-type* :abcl)
		             (uiop:symbol-call :prove-asdf 'run-test-system :chute/rdf/t)))
    :components ((:module test
			  :pathname "t/"
			  :components
			  ((:test-file "aes")
                           (:test-file "blob")
                           (:test-file "type")
			   (:test-file "snapshot")
			   (:test-file "config")
			   (:test-file "test")))))



