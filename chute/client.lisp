(in-package :chute)

(defmethod put-shard ((file pathname) &key (uri "blob/unqualified"))
  (time 
   (drakma:http-request uri
                        :method :put
                        :content-type "application/octet-stream"
                        :content file)))

(defmethod put-shard ((input stream) &key (uri "blob/unqualified"))
  (time
   (drakma:http-request uri
                        :method :put
                        :content input
                        :content-type "application/octet-stream")))

(defvar *debug-post-results* nil)
(defun parse-post-result (results)
  (setf *debug-post-results* results)
  (format nil "~a~a" (chute/config:uri-base) results))

(defun transfer-blob/http (blob-directory)
  ;;; POST the metadata
  (let ((post-result
         (drakma:http-request (chute/config:uri-base)
                              :method :post
                              :content-type "application/json"
                              :content (merge-pathnames "index.json" blob-directory))))
    ;; Transfer the blob shards
    ;; Only one shard for now.
    ;;; TODO use byte-range to dynamically adjust transfer rate inside a shard
    (let ((shard-uri (format nil "~a0" (parse-post-result post-result)))
          (shard-0-path (merge-pathnames "0" blob-directory)))
      (note "Putting shard to ~a" shard-uri)
      (with-open-file (shard-stream shard-0-path :element-type '(unsigned-byte 8))
        (put-shard shard-stream :uri shard-uri)))))


 


