(in-package :chute.test)
;;;; "doodles" of various tests

(defun make-test-blob (&key (directory (merge-pathnames "blob/" (uiop/stream:setup-temporary-directory))))
  "Create a test blob with random data returning the directory it was created within."
  (let ((metadata (make-instance 'metadata))
        (shard-size (random (expt 2 16))))
    (setf (size metadata) (* shard-size (shards metadata)))
    (ensure-directories-exist directory)
    (with-open-file (index (merge-pathnames "index.json" directory)
                           :direction :output
                           :if-exists :supersede)
      (cl-json:encode-json metadata index))
    (loop :for i :below (shards metadata)
       :doing (with-open-file (output (merge-pathnames (format nil "~a" i) directory)
                                      :direction :output
                                      :element-type 'unsigned-byte
                                      :if-exists :supersede)
                (with-open-file (input "/dev/random"
                                       :direction :input
                                       :element-type 'unsigned-byte)
                  (loop :for i :below shard-size
                     :doing (write-byte (read-byte input) output)))))
    directory))

;;; on quoth/blimp
(rt:deftest serialize.1
  (serialize
   #p"/opt/cloudfleet/data/.snapshot/20150922T154338Z"
   #p"/var/tmp/blob")
  t)
   
;;; Demonstrate that IRONCLAD block ciphers indeed retain state
(rt:deftest blocks.1
    (let ((cipher
         (ironclad:make-cipher :aes
                               :key
                               (make-array 16 :element-type '(unsigned-byte 8))
                               :mode :cfb
                               :initialization-vector
                               (make-array 16 :element-type '(unsigned-byte 8))))
        (cipher2
         (ironclad:make-cipher :aes
                               :key
                               (make-array 16 :element-type '(unsigned-byte 8))
                               :mode :cfb
                               :initialization-vector
                               (make-array 16 :element-type '(unsigned-byte 8))))
        (plain-1 (ironclad:ascii-string-to-byte-array "this"))
        (cipher-1 (make-array 4 :element-type '(unsigned-byte 8)))
        (plain-2 (ironclad:ascii-string-to-byte-array "open"))
        (cipher-2 (make-array 4 :element-type '(unsigned-byte 8))))
    (ironclad:encrypt cipher plain-1 cipher-1)
    (ironclad:encrypt cipher plain-2 cipher-2)
    (let ((cipher-12 (make-array 8 :element-type '(unsigned-byte 8)))
          (plain-12 (make-array 8 :element-type '(unsigned-byte 8))))
      (replace cipher-12 cipher-1 :start1 0 :end1 4 :start2 0)
      (replace cipher-12 cipher-2 :start1 4 :end1 8 :start2 0)
      (ironclad:decrypt cipher2 cipher-12 plain-12)
      (values cipher plain-1 cipher-1 plain-2 cipher-2 cipher-12 plain-12)))
    t)

(rt:deftest server.1
    (let ((directory (make-test-blob)))
      (chute.server:start-server)
      (prog1
          (multiple-value-list
           (put-file (merge-pathnames "0" directory)))
        (chute.server:stop-server)))
  t)

;;; A test for a running server
;;; (rt:do-test 'chute.test::put-request.1)
(rt:deftest put-request.1
    (if (not (chute.server:running-server-p))
        (error "No server running.")
        (let* ((directory (make-test-blob))
               (file (merge-pathnames "0" directory)))
          (put-file file)))
  t)
  





