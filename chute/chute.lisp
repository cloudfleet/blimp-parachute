(in-package :chute)

(defun backup ()
  ;;; create snapshot
  (let ((snapshot-path (snapshot))
        (blob-path (uiop:temporary-directory)))
  ;;; serialize snapshot to directory encrypting blob in-memory
    (serialize snapshot-path blob-path)
  ;;; get backup off system
    (transfer blob-path)))

(defun serialize (snapshot-path path)
  (ensure-directories-exist path)
  (let ((metadata (make-instance 'blob-metadata))
        (cipher (get-cipher :aes))
        (send-output (ironclad:make-octet-output-stream)))
    (declare (ignore cipher send-output))
    (if (mocked-p)
        (make-blob/mock)
        (progn 
          (with-open-file (stream (merge-pathnames "index.json" path) :direction :output)
            (cl-json:encode-json metadata stream))
          (btrfs/send snapshot-path))
        #+nil
        (encrypt-output send-output :cipher cipher))))

(defun encrypt (blob)
  (declare (ignore blob))
  (warn "Encyrption unimplemented."))

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

