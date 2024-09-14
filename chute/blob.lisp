(in-package :chute)

(defclass metadata ()
  ((version
    :initform "20240912a"
    :accessor metadata-version
    :documentation "Version of blob metadata.")
   (prototype
    :accessor metadata-prototype
    :initform '(("lispClass" ."metadata") ("lispPackage". "chute")))
   (node 
    :initform "localhost"
    :accessor metadata-node
    :documentation "Node creating this blob.")
   (domain
    :initform (alexandria:random-elt
               `("https://n3.not.org/chute/blob#"
                 ,(chute/io.cloudfleet:domain)))
    :accessor domain
    :documentation "Domain creating this blob.")
   (mount
    :initform (chute/config:path (chute/config:default))
    :accessor mount
    :documentation "Filesystem mount point of blob.")
   (timestamp
    :accessor timestamp
    :initform (timestamp-now))
   (parent
    :initform nil
    :accessor parent
    :documentation "Previous version of this blob, or nil if this is the first blob in a series.")
   (shards
    :initform 1
    :accessor shards
    :documentation "Number of pieces (shards) the blob is split across.")
   (size
    :accessor size
    :documentation "Size of blob in bytes.")
   (checksum
    :accessor checksum ;; TODO specify/switch algo
    :documentation "Checksum of blob.")
   (iv
    :accessor iv
    :documentation "Unique initialization Vector (iv) of encryption for this blob.")
   (nonce 
    :accessor nonce
    :documentation "Nonce for this access to blob contents.")
   (encrypted
    :initform nil ;;; ?? blobs can exist in staging areas locally unencrypted (local copy)
    :accessor encrypted-p
    :documentation "Whether the blob is in an encrypted state.")
   (uri
    :initform nil
    #+nil 
    `((:user . ,user)
      (:parent . ,parent)
      (:timestamp . ,timestamp)
      (:shards . ,shards)
      (:checksum . ,checksum)
      (:encrypted . ,encrypted)))))

(defgeneric stage-blob (file-or-directory)
  (:documentation "Stage the artifacts at pathname, possibly recursively for blob encryption.")
  (:method ((file-or-directory string))
    (stage-blob (pathname file-or-directory)))
  (:method ((file-or-directory pathname))
    (let ((blob-path (chute/fs:make-directory)))
      (flet ((make-blob-from-single-file (file blob-path)
               (with-open-file (input-stream file
                                             :direction :input
                                             :element-type '(unsigned-byte 8))
                 (make-blob input-stream blob-path))))
        (if
         (not 
          (equalp (pathname file-or-directory)
                  (uiop:ensure-directory-pathname file-or-directory)))
         (make-blob-from-single-file file-or-directory blob-path)
         (error "Unimplemented MAKE-BLOB of recursive input"))
        ;; make an UPDATE-METADATA?
        (let ((metadata (read-metadata blob-path :json)))
          (setf (timestamp metadata)
                (timestamp-now)

                (mount metadata) ;;; just use the PATHNAME
                file-or-directory)
          (transcribe-metadata blob-path metadata :json))))))

(defmethod make-blob ((input-stream stream) blob-path)
  "Make blob from INPUT-STREAM with output at BLOB-PATH"
  (ensure-directories-exist blob-path) ;; XXX should be done elsewhere, but I guess it can't hurt.
  (let* ((metadata (make-instance 'metadata))
         (aes-ctr (chute/crypt:get-key)) 
         (cipher (chute/crypt:get-cipher aes-ctr))
         (digest (ironclad:make-digest :sha256))
         (buffer-size 8192)
         (buffer (make-array buffer-size :element-type '(unsigned-byte 8)))
         (total-shard-bytes 0))
          ;;; TODO: how do we know the total size of the snapshot
          ;;; until we read all the bytes?  Until we figure this out
          ;;; we cannot shard without two passes through all the data
          ;;; serialize metadata containing key
    (with-open-file (output-stream (merge-pathnames "0" blob-path)
                                  :direction :output
                                  :if-exists :supersede 
                                  :element-type '(unsigned-byte 8))
      (loop
         :with input-stream-eof-p = nil
         :until input-stream-eof-p
         :do (multiple-value-bind (bytes eof-p b c d)
                 (chute/crypt:encrypt-from input-stream :buffer buffer :cipher cipher :digest digest)
               (declare (ignore b c d))
               (setf input-stream-eof-p eof-p)
               (incf total-shard-bytes bytes)
               (write-sequence buffer output-stream :start 0 :end bytes)))
      (setf (size metadata) total-shard-bytes
            (nonce metadata) (nonce aes-ctr)
            (checksum metadata) (ironclad:byte-array-to-hex-string
                                 (ironclad:produce-digest digest)))
      (transcribe-metadata blob-path metadata :json)
      (values blob-path metadata))))

(defgeneric transcribe-metadata (path metadata format)
  (:documentation "Serialize METADATA to PATHNAME in FORMAT.")
  (:method ((path pathname) (metadata metadata) (format (eql :json)))
    (with-open-file (stream
                     (merge-pathnames "index.json" path)
                     :direction :output
                     :if-exists :supersede)
      (cl-json:encode-json metadata stream)))
  (:method ((path pathname) (metadata metadata) (format (eql :n3)))
    (with-open-file (stream (merge-pathnames "index.n3" path))
      :direction :output
      :if-exists :supersede
      (error "Unimplemented serialization to N3 triples."))))

(defgeneric read-metadata (path format)
  (:documentation "Read metadata instance at PATH with default FORMAT being :json")
  (:method ((path pathname) (format (eql :json)))
    (with-open-file (stream (merge-pathnames "index.json" path))
      (cl-json:with-decoder-simple-clos-semantics (cl-json:decode-json stream)))))

;;; XXX this will read the ENTIRE BLOB into memory before returning a result
(defun decrypt-blob-as-octets (directory)
  "Decrypt the blob in DIRECTORY as a stream of bytes"
  (let* ((metadata (with-open-file (stream (merge-pathnames "index.json" directory))
                     (cl-json:with-decoder-simple-clos-semantics (cl-json:decode-json stream))))
         (aes-ctr (make-instance 'aes-ctr :nonce (slot-value metadata 'nonce)))
         (cipher (chute/crypt:get-cipher aes-ctr))
         (buffer (make-array (chute/config:buffer-size)
                             :element-type '(unsigned-byte 8))))
    (with-open-file (shard (merge-pathnames "0" directory)
                           :direction :input
                           :element-type '(unsigned-byte 8))
      (values
       (loop
          :with result = (make-array 0 :element-type '(unsigned-byte 8) :fill-pointer t :adjustable t)
          :with eof = nil
          :until eof
          :do (let ((bytes-read (read-sequence buffer shard)))
                (when (not (= bytes-read (chute/config:buffer-size)))
                  (setf eof t))
                (ironclad:decrypt-in-place cipher buffer :start 0 :end bytes-read)
                ;;; XXX one byte at a time? Optimize me!
                (loop :for i :below bytes-read
                   :doing (vector-push-extend (aref buffer i) result)))
          :finally (return result))
       metadata
       cipher))))

(defun timestamp-now ()
  (simple-date-time:|yyyymmddThhmmssZ| (simple-date-time:now)))

