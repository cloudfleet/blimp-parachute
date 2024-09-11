(in-package :chute/rsync)

(defclass rsync (fs)
  ((outputs :initform (defvar *rsyncs* nil))))

;;; one-way rsync (potentially destructive)
;;;
;;; If you have a remote user that can rsync over SSH, then you
;;; can use this method.  
(defun sink (from to)
  (warn "Attempting undebugged rsync from ~&<~a> to~&<~a>~%" local remote)
  (let* ((local
           (or 
            (uiop:pathname-directory-pathname from)
            (pathname from)))
         (remote
           (chute/uri:remote-uri to))
         (host
           (getf (pathname-host remote) :authority)
         (user
           (or
            "me" ;;; TODO better autoconfigure
            (uiop:getenv "USER")))
         (output
           (uiop:run-program '("rsync" "-avzP"
                               local
                               (format nil "~a@~a:.waste/~a"
                                user host (pathname-directory remote))
                               :output :string))))
    (values
     (pushnew
      output
      (slot-value *rsyncs* 'outputs)))))


         
         
    



