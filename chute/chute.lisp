(in-package :chute)

(defun client (&key (config "client-config.json"))
  "Main entrypoint for client."
  (note "Starting client configured via ~a" config)
  (get-client-config :file config :force t)
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
               (btrfs/subvolume/snapshot :path (path (get-client-config)))
             (note "Snapshot of '~a' with output ~&~a~& and error~&~a~&" snap-path out err)
             snap-path)))
    (unless (btrfs-snapshots)
      (snapshot))
    (loop
       :doing (progn
                (multiple-value-bind (sec min hour date month year daylight-p zone)
                    (get-decoded-time)
                  (when (and (= min 0)
                             (= hour 0))
                    (snapshot)
                    (sleep 60))
                  (sleep 31))))))

(defun transfer-task ()
  "Main task for transferring backups.")


     
       
  

