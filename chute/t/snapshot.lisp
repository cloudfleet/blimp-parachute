(in-package :cl-user)

(prove:plan 1)
(prove-ok
 (let ((snapshots (chute/fs:snapshots)))
   (chute:make-blob
    (last snapshots)
    (chute/fs:make-directory)))
 "Making a blob via defaults")


#|
(prove:plan 1)
(prove:ok
 (chute/config:with-cloudfleet-config
   (let ((snapshots (chute/fs:snapshots)))
     (unless snapshots
       (error "No snapshots to send."))
     (make-blob
      (first (reverse snapshots))
      (chute:make-new-directory))))
"Making a blob from a snapshot…")
#|


#|
;;; lowlevel test of btrfs send snapshot to stream
(prove:plan 1)
(prove:ok
 (chute/client:with-cloudfleet-config
    ;;; assuming there is at least one snapshot
   (let ((snapshots (chute/fs:snapshots)))
     (unless snapshots
       (error "No snapshots to send."))
     (let ((result (btrfs/send (first (snapshots))))) 
       (and
	(streamp result)
	(equal (stream-element-type result) '(unsigned-byte 8)))))))
    "Sending snapshot to stream…"
|#

(prove:finalize)
