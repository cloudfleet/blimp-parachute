(in-package :chute)

(defmethod put-blob ((file pathname) &key (uri "http://localhost:2001/blob/foo"))
  (drakma:http-request uri
                       :method :put
                       :content-type "application/octet-stream"
                       :content file))

(defmethod put-blob ((input stream) &key (uri "http://localhost:2001/blob/foo"))
  (declare (ignore uri))
  (warn "Unimplemented put file to stream."))
(defun transfer-blob (blob-directory &key (uri-base "http://localhost:2001/blob/"))
  ;;; POST the metadata
  (flet ((interpret-index-results (results)
           (warn "Unimplemented parse of POST.")
           (values
            (format nil "~a/~a" uri-base (strip-double-slash results)))))
  (let ((post-index-results
         (drakma:http-request uri-base
                              :method :post
                              :content-type "application/json"
                              :content (merge-pathnames "index.json" blob-directory))))
        ;;; Then transfer the shards
        ;;; Only one shard for now.
    ;;; TODO use byte-range to transfer portions
    (let ((blob-uri (interpret-index-results post-index-results))
          (shard-0-path (merge-pathnames "0" blob-directory)))
      (with-open-file (shard-stream shard-0-path :element-type '(unsigned-byte 8))
        (drakma:http-request blob-uri
                             :content shard-stream
                             :content-type "application/octet-stream"))))))


 


