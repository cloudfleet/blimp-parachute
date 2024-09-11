
(prove:plan 1)

(in-package :cl-user)

(defun make-blob/test (&key (directory (chute/fs:make-directory)))
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
                (with-open-file (input chute/config:*random-device*
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

(prove:ok
 (make-blob/test)
 "Making a blob.")

(prove:finalize)
