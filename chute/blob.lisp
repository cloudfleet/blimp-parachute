(in-package :chute)

(defclass metadata ()
  ((version
    :initform "2015111901"
    :documentation "Version of blob metadata.")
   (prototype
    :initform '(("lispClass" ."metadata") ("lispPackage". "chute")))
   (node
    :initform "urn:chute:node:0"
    :documentation "Node creating this blob.")
   (domain
    :initform "example.com"
    :documentation "Domain creating this blob.")
   (mount
    :initform *path*
    :documentation "Mount point of blob.")
   (timestamp
    :reader timestamp
    :initform (simple-date-time:rfc-2822 (simple-date-time:now)))
   (parent
    :initform nil
    :documentation "Previous blob, or nil if this is the first blob in a series.")
   (shards
    :initform 1
    :accessor shards
    :documentation "Number of pieces (shards) the blob is split across.")
   (size
    :accessor size
    :documentation "Size of blob in bytes.")
   (checksum
    :documentation "Checksum of blob.")
   (nonce 
    :accessor nonce
    :documentation "Nonce of block key.")))

(defmethod make-blob ((file pathname) blob-path)
  (with-open-file (input-stream file
                                :direction :input
                                :element-type '(unsigned-byte 8))
    (make-blob input-stream blob-path)))
  
(defmethod make-blob ((snapshot-path string) blob-path)
  (make-blob (btrfs/send snapshot-path) blob-path))

(defmethod make-blob ((input-stream stream) blob-path)
  (ensure-directories-exist blob-path)
  (let* ((shard-bytes 0)
         (metadata (make-instance 'metadata))
           ;;; TODO: initialize with AES key and random nonce
         (aes-ctr-key (get-key)) 
         (cipher (cipher aes-ctr-key))
         (buffer-size 8192)
         (buffer (make-array buffer-size :element-type '(unsigned-byte 8))))
          ;;; TODO: how do we know the total size of the snapshot
          ;;; until we read all the bytes?  Until we figure this out
          ;;; we cannot shard without two passes through all the data
          ;;; serialize metadata containing key
    (with-open-file (shard (merge-pathnames "0" blob-path)
                           :direction :output
                           :if-exists :supersede 
                           :element-type '(unsigned-byte 8))
      (loop
         :with eof = nil
         :until eof
         :do (let ((bytes-read (read-sequence buffer input-stream :start 0 :end buffer-size)))
               (when (not (= bytes-read buffer-size))
                 (setf eof t))
               (incf shard-bytes bytes-read)
               (ironclad:encrypt-in-place cipher buffer :start 0 :end bytes-read)
               (write-sequence buffer shard :start 0 :end bytes-read))))
    (setf (size metadata) shard-bytes)
    (setf (nonce metadata) (nonce aes-ctr-key))
    (with-open-file (stream (merge-pathnames "index.json" blob-path) :direction :output
                            :if-exists :supersede)
;;      (cl-json:with-substitute-printed-representation-restart (metadata stream) ;; XXX not working
      (cl-json:encode-json metadata stream))
    (values blob-path metadata)))

;;; XXX this will read the ENTIRE BLOB into memory before returning a result
(defun decrypt-blob-as-octets (directory)
  "Decrypt the blob in DIRECTORY as a stream of bytes."
  (let* ((metadata (with-open-file (stream (merge-pathnames "index.json" directory))
                     (cl-json:with-decoder-simple-clos-semantics (cl-json:decode-json stream))))
         (aes-ctr-key (make-instance 'aes-ctr-key :nonce (slot-value metadata 'nonce)))
         (cipher (cipher aes-ctr-key))
         (buffer-size 8192)
         (buffer (make-array buffer-size :element-type '(unsigned-byte 8))))
    (with-open-file (shard (merge-pathnames "0" directory)
                           :direction :input
                           :element-type '(unsigned-byte 8))
      (values 
       (loop
          :with result = (make-array 0 :element-type '(unsigned-byte 8) :fill-pointer t :adjustable t)
          :with eof = nil
          :until eof
          :do (let ((bytes-read (read-sequence buffer shard :start 0 :end buffer-size)))
                (when (not (= bytes-read buffer-size))
                  (setf eof t))
                (ironclad:decrypt-in-place cipher buffer :start 0 :end bytes-read)
                ;;; XXX one byte at a time? Optimize me!
                (loop :for i :upto bytes-read
                   :doing (vector-push-extend (aref buffer i) result)))
          :finally (return result))
       metadata
       cipher))))

(defun make-blob/test (&key (directory (merge-pathnames "blob/" (uiop/stream:setup-temporary-directory))))
  "Create a test blob with random data returning the directory it was created within."
  (let ((metadata (make-instance 'metadata))
        (shard-size (random (expt 2 16)))
        (blob-bytes 0))
    (setf (size metadata) (* shard-size (shards metadata)))
    (ensure-directories-exist directory)
    (note "Creating test blob under '~a'." directory)
    (loop :for i :below (shards metadata)
       :doing (with-open-file (output (merge-pathnames (format nil "~a" i) directory)
                                      :direction :output
                                      :element-type '(unsigned-byte 8)
                                      :if-exists :supersede)
                (with-open-file (input *random-device*
                                       :direction :input
                                       :element-type '(unsigned-byte 8))
                  (loop :for i :below shard-size
                     :doing (write-byte (read-byte input) output) ;; XXX slow:  use {WRITE,READ}-SEQUENCE
                     :doing (incf blob-bytes 1)))))
    (setf (size metadata) blob-bytes)
    (with-open-file (index (merge-pathnames "index.json" directory)
                           :direction :output
                           :if-exists :supersede)
      (cl-json:encode-json metadata index))
    directory))

