(in-package :chute.server)

(define-easy-handler (index :uri "/"
                            :default-request-type :get)
    nil
  (chute:note "uri: ~a" (request-uri*))
  (setf (content-type*) "text/plain")
  "Hello world!")

#|

@route POST "/blob/*"
@route PUT "/blob/:domain/:node/:mount/:timestamp/"

|#
(define-easy-handler (blob
                      :uri (lambda (request)
                             (cl-ppcre:scan "/blob" (request-uri* request)))
                      :default-request-type :post)
    nil

  (chute:note "uri: ~a" (request-uri*))
  (case (hunchentoot:request-method*)
    (:post
     (blob-post (request-uri*)))
    (:get
     "Unimplemented.")
    (:put
     (blob-put (request-uri*)))
    (otherwise
     (format nil "Unimplemented method ~a." (hunchentoot:request-method*)))))

(defparameter *last-metadata* nil)
(defun blob-post (uri)
  ;;; TODO check that the uri is "/blob"
  ;;; Create output directory
  (chute:note "Raw post data: ~a" (raw-post-data :force-text t))
  (let* ((metadata (cl-json:with-decoder-simple-clos-semantics
                     (cl-json:decode-json-from-string  (raw-post-data :force-text t))))
         (relative-local-path
          (chute:strip-double-slash (format nil "~a/~a/~a/~a/"
                                            (chute::domain metadata)
                                            (chute::node metadata)
                                            (chute::mount metadata)
                                            (chute::timestamp metadata))))
         (local-path
          (ensure-directories-exist (merge-pathnames relative-local-path chute:*blob-storage-dir*))))
    (setf *last-metadata* metadata)
    (chute:note "Metadata: ~a." metadata)

    (with-open-file (file
                     (merge-pathnames "index.json" local-path)
                     :direction :output
                     :if-exists :supersede) ;; XXX
      (format file (raw-post-data :force-text t)))
    (format nil "~a/~a" uri relative-local-path)))

(defun blob-put (uri)
  ;;; Get a writable stream to the object
  (let ((octets (raw-post-data :force-binary t)))
    (chute:note "octets: ~a..." (subseq octets 0 16))
  ;;; Slurp bytes, writing to disk

    nil)) ;; what do we return?

(defparameter *server* nil)
(defparameter *blob-directory* (asdf:system-relative-pathname :chute "../var/blob/"))

(defun running-server-p ()
  *server*)

(defun start-server ()
  (ensure-directories-exist chute:*blob-storage-dir*)
  (chute:note "Storing incoming blobs under ~a" chute:*blob-storage-dir*)
  (when (running-server-p)
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

