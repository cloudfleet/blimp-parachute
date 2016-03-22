(in-package :chute)

(defclass transfer ()
  ((snapshot
    :accessor snapshot
    :initarg :snapshot
    :documentation "Local snapshot corresponding to this transfer.")
   (creation-time
    :accessor creation-time
    :initform nil
    :documentation "Timestamp of snapshot creation.")
   (blob-path
    :accessor blob-path
    :initform nil
    :documentation "On-disk representation of snapshot as blob.")))

(defclass transfer-http (transfer)
   ((status
    :accessor status
    :initform nil
    :documentation "Transfer status of blob.  One of nil, :transferring, or :done")
    (uri
     :accessor uri
     :initform nil
     :documentation "URI of blob directory.")))

(defmethod transfer ((method (eql :http)) snapshot-path)
  (let ((blob-path (uiop:temporary-directory)))
    ;; serialize encrypted blob to path
    (make-blob snapshot-path blob-path)     
    (transfer-blob/http blob-path)))

(defun transcribe-transfers ()
  (let (transfers)
    (dolist (snapshot (btrfs-snapshots))
      (push
       (make-transfer snapshot)
       transfers))
    transfers))

(defun make-transfer (path)
  (let* ((info (btrfs-snapshot-info path))
         (date-string (gethash "Creation time" info))
         (result (make-instance 'transfer-http :snapshot path)))
    (setf (creation-time result)
          (cl-date-time-parser:parse-date-time date-string))
    result))

(defvar *transfers* nil)

(defun get-transfers (&key (force nil))
  (unless (or *transfers*
              force)
    (note "Initializing backup transfers record keeping.")
    (setf *transfers* (transcribe-transfers))))

