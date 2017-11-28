(in-package :cl-user)

(prove:plan 1)
(prove:ok
 (let ((directory (make-blob #p"/etc/passwd" (chute:make-new-directory)))
       (already-running-server-p (chute/server:running-server-p)))
   (unless already-running-server-p
     (chute/server:start-server))
   (prog1
       (let ((results
	      (multiple-value-list
	       (transfer-blob/http directory))))
            ;;; TODO add other checks for successful transfer
	 (= (second results) 201))
     (unless already-running-server-p
       (chute/server:stop-server))))
 "Transferring blob to server…")

(prove:finalize)






