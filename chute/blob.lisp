(in-package :chute)

(defclass metadata ()
  ((version
    :initform "2015121300"
    :accessor version
    :documentation "Version of blob metadata.")
   (prototype
    :initform '(("lispClass" ."metadata") ("lispPackage". "chute")))
   (node
    :initform (engineroom-node)
    :accessor node
    :documentation "Node creating this blob.")
   (domain
    :initform (engineroom-domain)
    :accessor domain
    :documentation "Domain creating this blob.")
   (mount
    :initform *path*
    :accessor mount
    :documentation "Mount point of blob.")
   (timestamp
    :reader timestamp
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
    :documentation "Whether the blob is in an encrypted state.")))

(defmethod make-blob ((file pathname) blob-path)
  (with-open-file (input-stream file
                                :direction :input
                                :element-type '(unsigned-byte 8))
    (make-blob input-stream blob-path)))
  
(defmethod make-blob ((snapshot-path string) blob-path)
  (make-blob (btrfs/send snapshot-path) blob-path))

(defmethod make-blob ((input-stream stream) blob-path)
  "Read from INPUT-STREAM with output at BLOB-PATH"
  (ensure-directories-exist blob-path) ;; XXX should be done elsewhere, but I guess it can't hurt.
  (let* ((total-shard-bytes 0)
         (metadata (make-instance 'metadata))
           ;;; TODO: initialize with AES key and random nonce
         (aes-ctr (get-key)) 
         (cipher (cipher aes-ctr))
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
                 (encrypt-from input-stream :buffer buffer :cipher cipher :digest digest)
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
  "Decrypt the blob in DIRECTORY as a stream of bytes."
  (let* ((metadata (with-open-file (stream (merge-pathnames "index.json" directory))
                     (cl-json:with-decoder-simple-clos-semantics (cl-json:decode-json stream))))
         (aes-ctr (make-instance 'aes-ctr :nonce (slot-value metadata 'nonce)))
         (cipher (cipher aes-ctr))
         (buffer (make-array (buffer-size) :element-type '(unsigned-byte 8))))
    (with-open-file (shard (merge-pathnames "0" directory)
                           :direction :input
                           :element-type '(unsigned-byte 8))
      (values
       (loop
          :with result = (make-array 0 :element-type '(unsigned-byte 8) :fill-pointer t :adjustable t)
          :with eof = nil
          :until eof
          :do (let ((bytes-read (read-sequence buffer shard)))
                (when (not (= bytes-read (buffer-size)))
                  (setf eof t))
                (ironclad:decrypt-in-place cipher buffer :start 0 :end bytes-read)
                ;;; XXX one byte at a time? Optimize me!
                (loop :for i :below bytes-read
                   :doing (vector-push-extend (aref buffer i) result)))
          :finally (return result))
       metadata
       cipher))))

(defun make-blob/test (&key (directory (make-new-directory)))
  "Create a test blob with random data returning the directory it was created within."
  (let ((metadata (make-instance 'metadata))
        (shard-size (random (expt 2 16)))
        (total-blob-bytes 0))
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
                     ;; XXX slow:  use {WRITE,READ}-SEQUENCE
                     :doing (write-byte (read-byte input) output) 
                     :doing (incf total-blob-bytes 1)))))
    (setf (size metadata) total-blob-bytes
          (encrypted-p metadata) nil)
    (with-open-file (index (merge-pathnames "index.json" directory)
                           :direction :output
                           :if-exists :supersede)
      (cl-json:encode-json metadata index))
    directory))

