(in-package :chute/rsync)

(defclass rsync (fs)
  ((outputs :initform (defvar *rsyncs* nil))))

;;; one-way rsync (potentially destructive)
;;;
;;; If one has a remote user on host that has permissions to rsync to
;;; via SSH, then you can use this method.
(defun sink (from to
               &key remote-user)
  (warn "Attempting undebugged rsync from ~&<~a> to~&<~a>~%" local remote)
  (let* ((local
           (truename from)) 
         (remote
           (chute/uri:remote-uri to))
         (host
           (getf (pathname-host remote) :authority))
         (user
           (or
            user
            (uiop:getenv "USER")
            "me" ;;; TODO better autoconfigure
            "kilroy"))
         (remote-rsync-command
           (format nil "~a@~a:.waste/~a"
                   remote-user
                   host
                   (pathname-directory remote)))
         (start
          (let ((note (format "Starting rsync from ~&<~a> to~&<~a>~%" local remote)))
            (note note)
            note))
         (output
           (uiop:run-program '("rsync" "-avzP"
                               local remote-rsync-command
                               :output :string))))
    (values 
     (pushnew output
              (slot-value *rsyncs* 'outputs))
     start
     (let ((note (format "Finished rsync")))
       (note note)
       note))))



         
         
    



