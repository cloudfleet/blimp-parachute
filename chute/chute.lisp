(in-package :chute)

(defparameter *path* "/opt/cloudfleet/data")
(defparameter *snapshot-base* "/opt/cloudfleet/data/.snapshot/")
(defparameter *btrfs-command* (asdf:system-relative-pathname :chute "../setup/btrfs"))
(defparameter *keystore* "/opt/cloudfleet/data/shared/chute/")

(defun backup ()
  ;;; create snapshot
  (let ((snapshot-path (snapshot))
        (blob-path (uiop:temporary-directory)))
  ;;; serialize snapshot to directory encrypting blob in-memory
    (serialize snapshot-path blob-path)
  ;;; get backup off system
    (transfer blob-path)))

(defun serialize (snapshot blob)
  (warn "Unimplemented serialize of ~s to ~s." snapshot blob))

(defun transfer (blob)
  (warn "Unimplemented transfer of ~s off system." blob))

(defun ensure-sanity ()
  (unless (probe-file *snapshot-base*)
    (error "No directory to create snapshots at ~s." *snapshot-base*))
  (unless (probe-file *btrfs-command*)
    (error "No setuid btrfs found at ~s." *btrfs-command*))
  t)

(defun snapshot ()
  "Make snapshot, returning path of generated snapshot."
  (multiple-value-bind (out err snap-path)
      (btrfs/subvolume/snapshot :path *path*)
    (note "Snapshot ~a with output ~a and error ~a" snap-path out err)
    snap-path))

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

;;; TODO output should either be a pathname or an output stream
(defun btrfs/send (&key (path *path*) (output (uiop/stream::get-temporary-file)))
  (ensure-sanity)
  (let ((command (format nil "~a send ~a | cat"
                         *btrfs-command*
                         path)))
    (handler-case
        (uiop:run-program command :output output :error :string)
      (t (error)
        (note "btfs send failed on cause ~a." error)
        (return-from btrfs/send nil)))))

(defun note (message-or-format &rest args)
  (format t 
          "~&~a ~a"
          (simple-date-time:rfc-2822 (simple-date-time:now))
          (apply 'format 
                 nil
                 message-or-format
                 (if args 
                     args
                     nil))))  

    
        
