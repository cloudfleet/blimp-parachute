(in-package :chute/crypt)

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
       (let ((key (or (chute/io.cloudfleet:engineroom-key)
                      (make-array 32 :element-type '(unsigned-byte 8)))))
         (ironclad:make-cipher :aes :mode :ctr
                               :key key
                               :initialization-vector
                               (make-array 16 :element-type '(unsigned-byte 8)))))
      ((eq type :aes)
       (let ((key (or (chute/io.cloudfleet:engineroom-key)
                      (make-array 32 :element-type '(unsigned-byte 8)))))
         (ironclad:make-cipher :aes :mode :cfb
                               :key key
                               :initialization-vector
                               (make-array 16 :element-type '(unsigned-byte 8)))))
      ((find type '(:salsa20 :salsa))
       (ironclad:make-cipher :salsa20 :mode :stream
                             :key (make-array 32 :element-type '(unsigned-byte 8))
                             :initialization-vector
                             (make-array 12 :element-type '(unsigned-byte 8)))))))

(defun get-key ()
  "Return an AES-CTR ready to be used."
  (let ((nonce (make-array 8 :element-type '(unsigned-byte 8))))
    (with-open-file (random "/dev/urandom"  :element-type '(unsigned-byte 8))
      (loop :for i :below 8
         :doing (setf (aref nonce i) (read-byte random))))
    (make-instance 'aes-ctr :nonce nonce)))

;;; XXX currently unused
(defun %get-some-stream-octets (stream size) 
  (let ((buffer (ironclad::buffer stream))
        (index (ironclad::index stream)))
    (setf (ironclad::index stream) (- index size)) 
    (setf (ironclad::buffer stream) (subseq buffer size index))
    (subseq buffer 0 size)))

(defclass key () nil)

(defclass aes-ctr (key)
  ((key :accessor key
        :type '((unsigned-byte 8) 32)
        :initform (or
                   (chute/io.cloudfleet:engineroom-key)
                   (progn
                     (warn "Creating null key.")
                     (make-array 32 :element-type '(unsigned-byte 8)))))
   (nonce :accessor nonce
          :type '((unsigned-byte 8) 8))
   (initialization-vector :accessor iv
                          :type '((unsigned-byte 8) 16))
   (cipher :accessor cipher)))
           

(defmethod shared-initialize :after ((result aes-ctr) slot-names &key (nonce nil nonce-p))
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


(defun encrypt-from (stream &key
                              (buffer (make-array (chute/config:buffer-size)
                                                  :element-type '(unsigned-byte 8)) buffer-p)
                              (cipher nil cipher-p)
                              (digest nil digest-p))
  "Encrypt bytes from STREAM with CIPHER into BUFFER updating DIGEST on encrypted contents."
  (let* ((b (if  buffer-p
                 buffer
                 (make-array (chute/config:buffer-size) :element-type '(unsigned-byte 8))))
         (bytes (read-sequence buffer stream)))
    (when (and cipher-p
               cipher)
      (ironclad:encrypt-in-place cipher b :start 0 :end bytes))
    (when digest-p
      (ironclad:update-digest digest b :start 0 :end bytes))
    (values
     bytes
     ;;EOF 
     (if (not (= bytes (length b)))
         t
         nil)
      b
      cipher
      digest)))

(defun decrypt-from (stream &key
                              (buffer (make-array (chute/config:buffer-size)
                                                  :element-type '(unsigned-byte 8)) buffer-p)
                              (cipher nil cipher-p)
                              (digest nil digest-p))
  "Decrypt bytes via CIPHER from STREAM into BUFFER updating DIGEST on unencrypted contents."
  (let* ((b (if  buffer-p
                buffer
                (make-array (chute/config:buffer-size) :element-type '(unsigned-byte 8))))
         (bytes (read-sequence b stream)))
    (when cipher-p
      (ironclad:decrypt-in-place cipher buffer :start 0 :end bytes))
    (when digest-p
      (ironclad:update-digest digest buffer :start 0 :end bytes))
    (values
     bytes
     ;; EOF
     (if (not (= bytes (length b)))
         t
         nil)
     buffer
     cipher
     digest)))
     
