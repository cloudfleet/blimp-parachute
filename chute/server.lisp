1(in-package :chute.server)

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
     (receive-blob-post (request-uri*)))
    (:get
     "Unimplemented.")
    (:put
     (receive-blob-put (request-uri*)))
    (otherwise
     (format nil "Unimplemented method ~a." (hunchentoot:request-method*)))))

(defparameter *debug-blob-post* nil) ;; DEBUG
(defun receive-blob-post (uri)
  ;;; TODO check that the uri is "/blob"
  (note "Raw post data: ~a" (raw-post-data :force-text t))
  (let* ((metadata (cl-json:with-decoder-simple-clos-semantics
                     (cl-json:decode-json-from-string  (raw-post-data :force-text t))))
         (relative-local-path (ensure-blob-path metadata))
         (local-path
          (ensure-directories-exist   ;; Create output directory
           (merge-pathnames (hunchentoot:url-decode relative-local-path) *blob-storage-directory*))))
    (setf *debug-blob-post* (list uri metadata relative-local-path local-path))
    (note "Blob post metadata: ~a." metadata)
    (with-open-file (file
                     (merge-pathnames "index.json" local-path)
                     :direction :output
                     :if-exists :supersede) ;; XXX
      (format file (raw-post-data :force-text t)))
    (setf (hunchentoot:content-type*) "text/plain")
    relative-local-path))

(defvar *debug-post-request* nil)
(defvar *debug-post-request* nil)
(defun receive-blob-put (uri)
  (setf *debug-post-request* hunchentoot:*request*)
  (note "Processing PUT for '~a'" uri)
  (let ((output-path (ensure-blob-path uri)))
    (when (null output-path)
      (setf (return-code*) hunchentoot:+http-bad-request+)
      (hunchentoot:abort-request-handler))
    (note "Writing data to '~a'" output-path)
    (ensure-directories-exist output-path)
    (let* ((octet-stream (raw-post-data :want-stream t))
           (total-bytes 0)
           (buffer-size 8192)
           (buffer (make-array buffer-size :element-type '(unsigned-byte 8))))
      (with-open-file (output output-path
                              :direction :output
                              :if-exists :supersede
                              :element-type '(unsigned-byte 8))
        (loop
           :with input-stream-eof-p = nil
           :until input-stream-eof-p
           :do (let ((bytes (read-sequence buffer octet-stream)))
                 (write-sequence buffer output :start 0 :end bytes)
                 (incf total-bytes bytes)
                 (when (not (= bytes (length buffer)))
                   (setf input-stream-eof-p t))))
        (note "DEBUG Finished reading ~a bytes" total-bytes)))
      
  (setf (hunchentoot:content-type*) "application/json"
          (return-code*) 201)
    "true")) ;; TODO return true when we actually can verify that a write occurred

(defmethod ensure-blob-path ((metadata chute:metadata))
  (chute:strip-double-slash (format nil "~a/~a/~a/~a/"
                                    (chute::domain metadata)
                                    (hunchentoot:url-encode (chute::node metadata))
                                    (chute::mount metadata)
                                    (chute::timestamp metadata))))

(defmethod ensure-blob-path ((uri string))
  "Return the path for which a blob should be stored according to URI."
  (when (search ".." uri)
    (note "Relative component in uri '~a' rejected." uri)
    (return-from ensure-blob-path nil))
  (when (let ((result (search *blob-uri-path* uri)))
          (not (and result
                    (= result 0))))
    (note "Uri '~a' does not start with '~a'" uri chute:*blob-uri-path*)
    (return-from ensure-blob-path nil))
  (warn "Unimplemented full check of local permission and hygiene for blob uri.")
  (merge-pathnames
   (subseq uri (length *blob-uri-path*))
   *blob-storage-directory*))

(defparameter *server* nil)

(defun running-server-p ()
  *server*)

(defun start-server ()
  (dolist (d `(,*blob-storage-directory* ,*log-directory*))
    (ensure-directories-exist d))
  (note "Storing incoming blobs under ~a" *blob-storage-directory*)
  (when (running-server-p)
    (warn "Stopping already present acceptor.")
    (stop *server*)
    (setf *server* nil))
  (setf *server* (make-instance 'http-acceptor :port *port*))
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


