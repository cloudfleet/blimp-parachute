(in-package :chute)

(defclass metadata ()
  ((version
    :initform "2015102901"
    :documentation "Version of blob metadata.")
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
    :documentation "Checksum of blob.")))

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

