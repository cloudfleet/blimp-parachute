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
(defun transfer-blob (blob-directory &key (post-uri (uri-base)))
  ;;; POST the metadata
  (flet ((interpret-post-results (results)
           (warn "Unimplemented parse of POST result.")
           (setf *debug-post-results* results)
           (format nil "~a~a" post-uri results)))
  (let ((post-index-results
         (drakma:http-request post-uri
                              :method :post
                              :content-type "application/json"
                              :content (merge-pathnames "index.json" blob-directory))))
    ;; Transfer the blob shards
    ;; Only one shard for now.
    ;;; TODO use byte-range to dynamically adjust transfer rate inside a shard
    (let ((shard-uri (format nil "~a0" (interpret-post-results post-index-results)))
          (shard-0-path (merge-pathnames "0" blob-directory)))
      (note "Putting shard to ~a" shard-uri)
      (with-open-file (shard-stream shard-0-path :element-type '(unsigned-byte 8))
        (put-shard shard-stream :uri shard-uri))))))


 


