(in-package :chute)

(defparameter *api-server* nil)

(defclass http-acceptor (restas:restas-acceptor)
  ()
  (:default-initargs
   :access-log-destination  (asdf:system-relative-pathname :chute "../var/log/api-access.log")
   :message-log-destination (asdf:system-relative-pathname :chute "../var/log/api-error.log")))

(defun start-api-server ()
  (ensure-directories-exist (asdf:system-relative-pathname :chute "../var/log/"))
  (restas:start (find-package :chute/api)
                :port (chute/config:api.port (chute/config:default))
                :acceptor-class 'http-acceptor))

(defun restart-api-server ()
  (restas:stop-all)
  (start-api-server))



