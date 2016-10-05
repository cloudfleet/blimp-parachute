(in-package :chute/server)

(defparameter *port* 2016)

(defparameter *blob-storage-directory*
  (asdf:system-relative-pathname :chute "../var/blob/"))

(defparameter *log-directory*
  (asdf:system-relative-pathname :chute "../var/log/"))

(defparameter *http-access-log*
  (merge-pathnames "access.log" *log-directory*))

(defparameter *http-error-log*
  (merge-pathnames "error.log" *log-directory*))

(defclass http-acceptor (hunchentoot:easy-acceptor)
  ()
  (:default-initargs
   :access-log-destination  *http-access-log* ;; TODO: configure via a lookup mechanism w/o specials
   :message-log-destination *http-error-log*))

