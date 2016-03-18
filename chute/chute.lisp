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
                            #'transfer-task)
     (lparallel:submit-task channel
                            (lambda () (sleep 360) (note "Slept for an hour.  Whatsup?"))))))

(defun transfer-task ()
  "Main task for transferring backups.")

(defun snapshot-task ()
  "Make snapshot of configured btrfs subvolume, returning path of generated snapshot."
  (multiple-value-bind (out err snap-path)
      (btrfs/subvolume/snapshot :path (path (get-client-config)))
    (note "Snapshot of '~a' with output ~&~a~& and error~&~a~&" snap-path out err)
    snap-path))

