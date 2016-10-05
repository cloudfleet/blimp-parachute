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
  (let* ((var-root
          (ensure-directories-exist chute/config:*blobs-directory*))
         (directory-as-file
          (pathname (cl-fad:open-temporary
                     :template (namestring (merge-pathnames "blob-%" var-root)))))
         (directory-with-file
          (pathname (namestring (concatenate 'string
                                             (namestring directory-as-file)
                                             "/foo")))))
    (delete-file directory-as-file)
    (ensure-directories-exist directory-with-file)
    (pathname (concatenate 'string (namestring directory-as-file) "/"))))

       
       
       
