(in-package :cl-user)

(prove:plan 1)
(prove:is
    (let ((key-1 (chute/crypt:get-cipher :aes-ctr))
          (key-2 (chute/crypt:get-cipher :aes-ctr))
          (plain-1 (ironclad:ascii-string-to-byte-array "this"))
          (cipher-1 (make-array 4 :element-type '(unsigned-byte 8)))
          (plain-2 (ironclad:ascii-string-to-byte-array "open"))
          (cipher-2 (make-array 4 :element-type '(unsigned-byte 8)))
          (plain-12 (make-array 8 :element-type '(unsigned-byte 8))))
      (ironclad:encrypt key-1 plain-1 cipher-1)
      (ironclad:encrypt key-1 plain-2 cipher-2)
      (ironclad:decrypt key-2
                        (concatenate '(vector (unsigned-byte 8) *) cipher-1 cipher-2)
                        plain-12)
      (list
       (= (+ (length plain-1) (length plain-2))
          (length plain-12))
       (loop
          :for i :across (concatenate '(vector (unsigned-byte 8) *) plain-1 plain-2)
          :for j :across plain-12
          :unless (eq i j)
          :return nil
          :finally (return t))))
    '(t t)
  "Testing that AES block ciphers as implemented by IRONCLAD indeed retain state~
    D( E(x1) || E(x2) ) == x1 || x2")

(prove:finalize)
