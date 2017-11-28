(in-package :cl-user)

(prove:plan 1)
(prove:ok
 (let ((instance (make-instance 'chute/config:client)))
   (find (slot-value instance
                       'chute/config:backing-store)
           '(:rsync :zfs :btfs)))
 "Making instance of the the client config from CLOS…")

(prove:finalize)
