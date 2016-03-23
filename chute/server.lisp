(in-package :chute.server)

(defvar *debug-last-request* nil)

(define-easy-handler
    (index :uri "/"
           :default-request-type :get)
    nil

  (chute:note "uri: ~a" (request-uri*))
  (setf (content-type*) "text/plain")
  "Hello world!")

(define-easy-handler
    (blob
     :uri (lambda (request)
            (cl-ppcre:scan (format nil "^~a" *blob-uri-path*) (request-uri* request))))
    nil

  (setf *debug-last-request* *request*)
  (chute:note "Request uri: '~a'" (request-uri*))

  (case (hunchentoot:request-method*)
    (:post
     (receive-blob-post (request-uri*)))
    (:get
     (receive-blob-get (request-uri*)))
    (:put
     (receive-blob-put (request-uri*)))
    (otherwise
     (format nil "Unimplemented method ~a." (hunchentoot:request-method*)))))

(defun receive-blob-post (uri)
    (unless (string-equal uri
                        *blob-uri-path*)
      (setf (return-code*) hunchentoot:+http-bad-request+)
      (note "ERROR: POST request to invalid uri.")
      (hunchentoot:abort-request-handler))
  (note "DEBUG: Raw POST data: ~a" (raw-post-data :force-text t))
  (let* ((metadata (cl-json:with-decoder-simple-clos-semantics
                     (cl-json:decode-json-from-string (raw-post-data :force-text t))))
         (relative-local-path (blob-path metadata))
         (local-path
          (ensure-directories-exist   
           (merge-pathnames relative-local-path *blob-storage-directory*))))
    (note "POST metadata parsed as: ~a." metadata)
    (with-open-file (file
                     (merge-pathnames "index.json" local-path)
                     :direction :output
                     :if-exists :supersede) ;; XXX
      (format file (raw-post-data :force-text t)))
    (setf (hunchentoot:content-type*) "text/plain")
    relative-local-path))

(defun receive-blob-put (uri)
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
    "true"))

(defun blob-path (metadata)
  (format nil "~a/~a/"
          (chute::domain metadata)
          (ironclad:byte-array-to-hex-string
           (ironclad:digest-sequence :sha256
                                     (flexi-streams:string-to-octets
                                      (cl-json:encode-json-to-string metadata))))))

(defun ensure-blob-path (path)
  (when (search ".." path)
    (note "Relative component in path '~a' rejected." path)
    (return-from ensure-blob-path nil))
  (when (let ((result (search *blob-uri-path* path)))
          (not (and result
                    (= result 0))))
    (note "Path '~a' does not start with '~a'" path chute:*blob-uri-path*)
    (return-from ensure-blob-path nil))
  (warn "Unimplemented full check of local permission and hygiene for blob uri.")
  (merge-pathnames
   (subseq path (length *blob-uri-path*))
   *blob-storage-directory*))

#|
    -->  GET /chute/blob/<URI>/index.json              
    nil
    <--  200 Original or 304 Not Modified or [45]xx Error
    (index.json) or nil

    -->  GET /chute/blob/<URI>/0
    <--  200 
    (json) SHA256 Hash
|#

(defun receive-blob-get (path)
  (declare (ignore path))
  (warn "Unimplemented RECIEVE-BLOB-PUT.")
  (setf (return-code*) +http-not-implemented+)
  (hunchentoot:abort-request-handler))

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


