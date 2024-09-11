(in-package :chute/rsync)

(eval-when (:execute)
  (warn "RSYNC unimplemented."))

(defclass rsync (fs)
  ((unimplemented)))

;;; one-way rsync (potentially destructive remotely)
(defun sink (local remote)
  (warn "Attempting undebugged rsync from ~&<~a> to~&<~a>~%" local remote)
  (values 
   (uiop:run-program
    '("rsync" "-avzP" local remote)
    :output :string)))


