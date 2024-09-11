(in-package :chute)

(defun transfer-task ()
  (warn "Main task for transferring backups unimplemented."))

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
