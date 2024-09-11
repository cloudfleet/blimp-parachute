(in-package :chute)

(defclass metadata ()
  ((version
    :initform "20240911a"
    :accessor version
    :documentation "Version of blob metadata.")
   (prototype
    :initform '(("lispClass" ."metadata") ("lispPackage". "chute")))
   (node
    :initform #+nil (chute/io.cloudfleet:node)
              (chute:chute.not.org)
    :accessor node
    :documentation "Node creating this blob.")
   (domain
    :initform (chute:chute.not.org) ;;; named in node?
    :accessor domain
    :documentation "Domain creating this blob.")
   (mount
    :initform (chute/config:path (chute/config:default))
    :accessor mount
    :documentation "Filesystem mount point of blob.")
   (timestamp
    :accessor timestamp
    :initform (simple-date-time:|yyyymmddThhmmssZ| (simple-date-time:now)))
   (parent
    :initform nil
    :accessor parent
    :documentation "Previous blob, or nil if this is the first blob in a series.")
   (shards
    :initform 1
    :accessor shards
    :documentation "Number of pieces (shards) the blob is split across.")
   (size
    :accessor size
    :documentation "Size of blob in bytes.")
   (checksum
    :accessor checksum
    :documentation "Checksum of blob.")
   (nonce 
    :accessor nonce
    :documentation "Nonce of block key.")
   (encrypted
    :initform t 
    :accessor encrypted-p
    :documentation "Whether the blob is in an encrypted state.")
   (uri
    :initform `((:user . ,user)
                (:parent . ,parent)
                (:timestamp . ,timestamp)
                (:shards . ,shards)
                (:checksum . ,checksum)
                (:nonce . ,nonce)
                (:encrypted . ,encrypted)))))
               
(defmethod make-blob ((file-or-directory pathname) blob-path)
  "Create blob from FILE-OR-DIRECTORY at BLOB-PATH"
  (flet ((read-file (file blob-path)
           (with-open-file (input-stream file
                                         :direction :input
                                         :element-type '(unsigned-byte 8))
             (make-blob input-stream blob-path))))
    (if
     (not 
      (equalp (pathname file-or-directory)
              (uiop:ensure-directory-pathname file-or-directory))
      (read-file file-or-directory pathname))
     (error "Unimplemented MAKE-BLOB of recursive input"))))

(defmethod make-blob ((snapshot-path string) blob-path)
  (prog1
      (make-blob (chute/fs:send snapshot-path) blob-path)
    ;; The following shenanigans are just to set the blob timestamp to
    ;; the creation time.  Obviously we should redo the API for making
    ;; a blob somehow.
    (let ((transfer (make-transfer snapshot-path))
          (metadata (with-open-file (stream (merge-pathnames "index.json" blob-path))
                      (cl-json:with-decoder-simple-clos-semantics (cl-json:decode-json stream)))))
      (setf (timestamp metadata)
            (creation-time transfer)

            (mount metadata)
            (chute/fs:snapshot/mount snapshot-path))
      (with-open-file (stream (merge-pathnames "index.json" blob-path) :direction :output
                              :if-exists :supersede)
        (cl-json:encode-json metadata stream)))))

(defmethod make-blob ((input-stream stream) blob-path)
  "Make blob from INPUT-STREAM with output at BLOB-PATH"
  (ensure-directories-exist blob-path) ;; XXX should be done elsewhere, but I guess it can't hurt.
  (let* ((total-shard-bytes 0)
         (metadata (make-instance 'metadata))
         (aes-ctr (chute/crypt:get-key)) 
         (cipher (chute/crypt:get-cipher aes-ctr))
         (digest (ironclad:make-digest :sha256))
         (buffer-size 8192)
         (buffer (make-array buffer-size :element-type '(unsigned-byte 8))))
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
      (with-open-file (stream (merge-pathnames "index.json" blob-path) :direction :output
                              :if-exists :supersede)
        (cl-json:encode-json metadata stream))
      (values blob-path metadata))))

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


