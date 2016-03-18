(in-package :chute)

(defclass transfer ()
  ((snapshot
    :accessor snapshot
    :initarg :snapshot
    :documentation "Local snapshot corresponding to this transfer.")
   (blob-path
    :accessor blob-path
    :initform nil
    :documentation "On-disk representation of snapshot as blob.")))

(defclass transfer-http (transfer)
   ((status
    :accessor status
    :initform nil
    :documentation "Transfer status of blob.  One of nil, :transferring, or :done")))

(defmethod transfer ((method (eql :http)) snapshot-path)
  (let ((blob-path (uiop:temporary-directory)))
    ;; serialize encrypted blob to path
    (make-blob snapshot-path blob-path)     
    (transfer-blob/http blob-path)))

(defun transcribe-transfers ()
  (let (transfers)
    (dolist (snapshot (btrfs-snapshots))
      (push
       (make-instance 'transfer-http
                      :snapshot snapshot)
       transfers)
      transfers)))

