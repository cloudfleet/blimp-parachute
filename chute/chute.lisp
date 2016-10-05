(in-package :chute)

(defun client (&key (config "client-config.json"))
  "Boot the chute client computation locally."
  (note "Starting client configured via ~a" config)
  (chute/config:default :file config :force t)
  (start-api-server)
  (if lparallel:*kernel*
      (warn "lparallel kernel unexpectedly present.")
      (setf lparallel:*kernel* (lparallel:make-kernel 3)))
  (let ((channel (lparallel:make-channel)))
    (values
     channel
     (lparallel:submit-task channel
                            #'snapshot-task) 
     (lparallel:submit-task channel
                            #'transfer-task))))

(defun snapshot-task ()
  (flet ((snapshot ()
           (multiple-value-bind (out err snap-path)
               (chute/fs:snapshot :path (chute/config:path (chute/config:default)))
             (note "Snapshot of '~a' with output ~&~a~& and error~&~a~&" snap-path out err)
             snap-path)))
    (unless (chute/fs:snapshots)
      (chute/fs:snapshot))
    (loop
       :doing (progn
                (multiple-value-bind (sec min hour date month year daylight-p zone)
                    (get-decoded-time)
                  (declare (ignore sec date month year daylight-p zone))
                  (when (and (= min 0)
                             (= hour 0))
                    (snapshot)
                    (sleep 60))
                  (sleep 31))))))

(defun transfer-task ()
  (warn "Main task for transferring backups."))



     
       
  

