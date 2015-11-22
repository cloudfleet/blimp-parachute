(in-package :chute)

(defun backup ()
  "Main task for backup.  To be run periodically on queue."
  (let ((snapshot-path (snapshot)) ;; create snapshot
        (blob-path (uiop:temporary-directory)))
    (make-blob snapshot-path blob-path)     ;; serialize encrypted blob to path
    (prog1
        (transfer-blob blob-path)     ;; get blob off system
      (warn "Unimplemented cleanup of blob at ~a" blob-path))))

(defun ensure-sanity ()
  (unless (probe-file *snapshot-base*)
    (error "No directory to create snapshots at ~s." *snapshot-base*))
  (unless (probe-file *btrfs-command*)
    (error "No setuid btrfs found at ~s." *btrfs-command*))
  #+abcl
  (probe-file *uri-base*)
  t)

(defun snapshot ()
  "Make snapshot of btfs volume at *path*, returning path of generated snapshot."
  (multiple-value-bind (out err snap-path)
      (btrfs/subvolume/snapshot :path *path*)
    (note "Snapshot ~a with output ~a and error ~a" snap-path out err)
    snap-path))

(defun make-new-directory ()
  (ensure-directories-exist *blobs-directory*)
  (let* ((directory
          #+ccl
           (subseq *blobs-directory* 0 (1- (length *blobs-directory*)))
           #-ccl *blobs-directory*)
          (file (uiop/stream::get-temporary-file :directory directory)))
    (delete-file file)
    (ensure-directories-exist
     (pathname (concatenate 'string (namestring file) "/")))))
