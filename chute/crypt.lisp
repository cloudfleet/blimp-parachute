(in-package :chute)

(defmethod encrypt-output ((send-output t) &key (cipher 'aes))
    (let ((size 8192))
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
                           (cipher-text (make-array 8192 :element-type '(unsigned-byte 8))))
                       (ironclad:encrypt cipher plain-text cipher-text)
                       cipher-text)))))

(defun get-cipher ()
  ;;; TODO: initialize cipher correctly
  (ironclad:make-cipher :aes
                        :key
                        (make-array 16 :element-type '(unsigned-byte 8))
                        :mode :cfb
                        :initialization-vector
                        (make-array 16 :element-type '(unsigned-byte 8))))

(defun %get-some-stream-octets (stream size) 
  (let ((buffer (ironclad::buffer stream))
        (index (ironclad::index stream)))
    (setf (ironclad::index stream) (- index size)) 
    (setf (ironclad::buffer stream) (subseq buffer size index))
    (subseq buffer 0 size)))


