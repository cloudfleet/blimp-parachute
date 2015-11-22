(in-package :chute)

(defun strip-double-slash (string)
  (let ((input (if (pathnamep string) (namestring string) string))
        (result (make-string-output-stream)))
    (loop
       :with previous-char-was-slash-p = nil
       :for c :across input
       :unless (and (eq c #\/)
                previous-char-was-slash-p)
       :do (progn 
             (setf previous-char-was-slash-p (eq c #\/))
             (write-char c result)))
    (get-output-stream-string result)))
       
         
       
       
       
