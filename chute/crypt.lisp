(in-package :chute)

(defun encrypt-output (send-output
                       &key (cipher (get-cipher :aes)))
  (flet ((cipher-type () :aes))
  (let ((size (cond
                ((eq (cipher-type) :aes) 8192)
                (t 64))))
      (loop
       :collecting (let ((buffer (ironclad::buffer send-output))
                         (index (ironclad::index send-output)))
                     (note "buffer size: ~a~,8@tindex: ~a~%" (length buffer) index)
                     (setf (ironclad::index send-output) (- index size))
                     (setf (ironclad::buffer send-output)
                           (if (> index size)
                               (subseq buffer size index)
                               buffer))
                     (let ((plain-text (if (> index size)
                                           (subseq buffer 0 size)
                                           buffer))
                           (cipher-text (make-array size :element-type '(unsigned-byte 8))))
                       (ironclad:encrypt cipher plain-text cipher-text)
                       cipher-text))))))

(defun get-cipher (arg) 
  "Return encryption source keyed for CIPHER"
  ;;; TODO: initialize cipher correctly
  (declare (keyword arg))
  (let ((type arg))
  (cond
    ((and (keywordp type)
          (eq type :aes-ctr))
     (ironclad:make-cipher :aes :mode :ctr
                           :key (make-array 32 :element-type '(unsigned-byte 8))
                           :initialization-vector (make-array 16 :element-type '(unsigned-byte 8))))
     ((find type '(:salsa20 :salsa))
      (ironclad:make-cipher :salsa20 :mode :stream
                            :key (make-array 32 :element-type '(unsigned-byte 8))
                            :initialization-vector (make-array 12 :element-type '(unsigned-byte 8)))))))

(defun get-key ()
  "Return an AES-CTR-KEY ready to be used."
  ;;; TODO read in key from known location
  ;;; TODO use a random nonce
  (make-instance 'aes-ctr-key :nonce #(2 0 0 0 0 0 0 1)))

;;; XXX currently unused
(defun %get-some-stream-octets (stream size) 
  (let ((buffer (ironclad::buffer stream))
        (index (ironclad::index stream)))
    (setf (ironclad::index stream) (- index size)) 
    (setf (ironclad::buffer stream) (subseq buffer size index))
    (subseq buffer 0 size)))


(defclass key () nil)

(defclass aes-ctr-key (key)
  ((key :accessor key
        :type '((unsigned-byte 8) 32)
        :initform (make-array 32 :element-type '(unsigned-byte 8)))
   (nonce :accessor nonce
          :type '((unsigned-byte 8) 8))
   (initialization-vector :accessor iv
                          :type '((unsigned-byte 8) 16))
   (cipher :accessor cipher)))
           

(defmethod shared-initialize :after ((result aes-ctr-key) slot-names &key (nonce nil nonce-p))
  (declare (ignore slot-names))
  (unless nonce-p
    (error "No nonce supplied for initialization."))
  (when nonce-p
    (setf (nonce result) nonce)
    (setf (iv result) (make-array 16 :element-type '(unsigned-byte 8)))
    (loop :for i :upto 7
       :doing (setf (aref (iv result) i)
                    (aref nonce i)))
;;    (setf (cipher result) (get-cipher :aes-ctr-mode))
    (setf (cipher result)
          (ironclad:make-cipher :aes :mode :ctr :key (key result) :initialization-vector (iv result))))
  result)



