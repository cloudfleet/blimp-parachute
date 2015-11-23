(in-package :chute)

(defun client ()
  "Main loop for client."
  (note "Client starting up.")
  (if lparallel:*kernel*
      (warn "lparallel kernel unexpectedly present.")
      (setf lparallel:*kernel* (lparallel:make-kernel 3)))
  (let ((backup-channel (lparallel:make-channel)))
    (note "Performing single backup task.")
    (values
     (lparallel:submit-task backup-channel #'backup)
     (lparallel:submit-task backup-channel (lambda () (sleep 360) (note "Slept for an hour.  Whatsup?")))
     backup-channel)))

(defun backup ()
  "Main task for backup.  To be run periodically on queue."
  (let ((snapshot-path (snapshot)) ;; create snapshot
        (blob-path (uiop:temporary-directory)))
    (make-blob snapshot-path blob-path)     ;; serialize encrypted blob to path
    (prog1
        (transfer-blob blob-path)     ;; get blob off system
      (warn "Unimplemented cleanup of blob at ~a" blob-path))))

(defun snapshot ()
  "Make snapshot of btfs volume at *path*, returning path of generated snapshot."
  (multiple-value-bind (out err snap-path)
      (btrfs/subvolume/snapshot :path *path*)
    (note "Snapshot ~a with output ~a and error ~a" snap-path out err)
    snap-path))

