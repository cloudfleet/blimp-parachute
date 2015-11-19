(in-package :chute)

(defun backup ()
  ;;; create snapshot
  (let ((snapshot-path (snapshot))
        (blob-path (uiop:temporary-directory)))
  ;;; serialize snapshot to directory encrypting blob in-memory
    (make-blob snapshot-path blob-path)
  ;;; get backup off system
    (transfer blob-path)))


(defun encrypt (blob)
  (declare (ignore blob))
  (warn "Encryption unimplemented."))

(defun transfer (blob)
  (warn "Untested transfer of ~s off system." blob)
  (drakma:http-request
   (format nil "~a/not.org/t/~a/~a"
           *uri-base*
           (timestamp blob)
           (random (expt 2 128)))
   :method :put
   :content-type "application/octet-stream"
   :stream (encrypt blob)))

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
