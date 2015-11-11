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
    (declare (ignore metadata)) ;; FIXME
    #+nil
    (with-open-file (stream (merge-pathnames "index.json" path) :direction :output)
      (cl-json:encode-json metadata stream))
    ;;; sbcl only
    (btrfs/send :snapshot-path snapshot-path)
    (encrypt-output send-output :cipher cipher)))

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
  "Make snapshot, returning path of generated snapshot."
  (multiple-value-bind (out err snap-path)
      (btrfs/subvolume/snapshot :path *path*)
    (note "Snapshot ~a with output ~a and error ~a" snap-path out err)
    snap-path))

(defun snapshot/mock ()
  (prog1 
      (make-blob/mock)
    (note "Snapshot mock created at ~a.")))

(defun btrfs/subvolume/snapshot (&key (path *path*))
  (ensure-sanity)
  (let* ((output (make-string-output-stream))
         (error (make-string-output-stream))
         (timestamp (simple-date-time:|yyyymmddThhmmssZ| (simple-date-time:now)))
         (snapshot-path (concatenate 'string *snapshot-base* timestamp))
         (snapshot (format nil "~a subvolume snapshot -r ~a ~a"
                           *btrfs-command*
                           path snapshot-path)))
    (uiop:run-program snapshot :output output :error error)
    (values 
     (get-output-stream-string output)
     (get-output-stream-string error)
     snapshot-path)))

;;;; TODO Need command to figure out latest generation

(defun btrfs/subvolume/find-new (&key (path *path*) generation)
  (ensure-sanity)
  (let* ((o (make-string-output-stream))
         (find-new (format nil "~a subvolume find-new ~a ~a"
                           *btrfs-command*
                           path generation)))
    (uiop:run-program find-new :output o)
    (get-output-stream-string o)))

(defun btrfs/subvolume/show (&key (path *path*))
  (ensure-sanity)
  (with-output-to-string (output)
    (with-output-to-string (error)
      (let ((command (format nil "~a subvolume show ~a"
                             *btrfs-command*
                             path)))
        (handler-case 
            (uiop:run-program command :output output :error error)
          (t (e)
            (declare (ignore e))
            (return-from btrfs/subvolume/show (values nil output error)))))
      (values t output error))))

(defun btrfs/send (&key (snapshot-path *path*))
  "Returns the stream containing the output of the btrfs/send operation on SNAPSHOT-PATH."
  (ensure-sanity)
  (let ((command (format nil "~A send ~A" *btrfs-command* snapshot-path)))
    (handler-case
      #+sbcl
      ;; XXX EH, could be that command/args really do need to be split
      (sb-ext:run-program command nil :wait nil :output :stream)
      #-sbcl
      (uiop/run-program:run-program command args :output :stream)
      (t (error)
        (note "btfs send failed with '~a'." error)
        (return-from btrfs/send nil)))))


