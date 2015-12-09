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
       

(defun make-new-directory ()
  (ensure-directories-exist *blobs-directory*)
  (let* ((directory
          #+ccl
           (subseq *blobs-directory* 0 (1- (length *blobs-directory*)))
           #-ccl *blobs-directory*)
          (file (uiop/stream::get-temporary-file :prefix "blob" :directory directory)))
    (delete-file file)
    (ensure-directories-exist
     (pathname (concatenate 'string (namestring file) "/")))))


       
       
       
