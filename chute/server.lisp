(in-package :chute.server)

(define-easy-handler (index :uri "/"
                            :default-request-type :get)
    nil
  (setf (content-type*) "text/plain")
  "Hello world!")

;; @route PUT "/blob/:timestamp/:hash/:n-bytes/:mth-window"
(define-easy-handler (put
                      :uri (lambda (request)
                             (cl-ppcre:scan "/blob" (request-uri* request)))
                      :default-request-type :put)
    nil
  ;;; Locate output directory
  ;;; Get a writable stream to the object
  ;;; Slurp bytes, writing to disk
  (setf (content-type*) "text/plain") ;; debugging
  (warn "Unimplemented PUT-BYTE-RANGE.")
  (format nil "uri: ~a" (request-uri*)))

(defun start-server ()
  (start (make-instance 'hunchentoot:easy-acceptor :port 2001)))

