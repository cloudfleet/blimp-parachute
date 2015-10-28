;;;; "doodles" of various tests

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
    t






