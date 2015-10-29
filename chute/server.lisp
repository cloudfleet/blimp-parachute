(in-package :chute.server)

(define-easy-handler (index :uri "/"
                            :default-request-type :get)
    nil
  (setf (content-type*) "text/plain")
  "Hello world!")

;; @route PUT "/blob/*"
;; @route PUT "/blob/:timestamp/:hash/:n-bytes/:mth-window"
(define-easy-handler (put
                      :uri (lambda (request)
                             (cl-ppcre:scan "/blob" (request-uri* request)))
                      :default-request-type :post)
    nil

  (chute:note "uri: ~a" (request-uri*))
  ;;; Locate output directory
  ;;; Get a writable stream to the object
  (let ((octets (raw-post-data :force-binary t)))
    (chute:note "octets: ~a" octets ))
  ;;; Slurp bytes, writing to disk

  nil) ;; what do we return?  

(defparameter *server* nil)
(defparameter *blob-directory* (asdf:system-relative-pathname :chute "../var/blob/"))

(defun running-server-p ()
  *server*)

(defun start-server ()
  (ensure-directories-exist *blob-directory*)
  (when *server*
    (warn "Stopping already present acceptor.")
    (stop *server*)
    (setf *server* nil))
  (setf *server* (make-instance 'hunchentoot:easy-acceptor :port 2001))
  (start *server*))

(defun stop-server ()
  (if *server*
      (progn
        (stop *server*)
        (setf *server* nil))
      (warn "No server found to stop.")))

(defun restart-server ()
  (stop-server)
  (start-server))

