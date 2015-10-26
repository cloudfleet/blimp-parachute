(in-package :chute)

(declaim (optimize (debug 3)))

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

(defclass blob-metadata ()
  ((version
   :initform "2015092401"
   :documentation "Version of blob metadata.")
  (node
   :documentation "Node creating this blob.")
  (domain
   :documentation "Domain creating this blob.")
  (mount
   :initform *path*
   :documentation "Mount point of blob.")
  (date
   :initform (simple-date-time:rfc-2822 (simple-date-time:now)))
  (parent 
   :documentation "Previous blob, or nil if this is the first blob in a series.")
  (checksum
   :documentation "Checksum of blob.")))

(defun serialize (snapshot-path path)
  (ensure-directories-exist path)
  (let ((metadata (make-instance 'blob-metadata))
        (cipher (get-cipher))
        (send-output (ironclad:make-octet-output-stream)))
    #+nil
    (with-open-file (stream (merge-pathnames "index.json" path) :direction :output)
      (cl-json:encode-json metadata stream))
    ;;; sbcl only
    (btrfs/send :snapshot-path snapshot-path)
    #+nil
    (loop
       :collecting (let ((buffer (ironclad::buffer send-output))
                         (index (ironclad::index send-output)))
                     (format t "buffer size: ~a~,8@tindex: ~a~%" (length buffer) index)
                     (setf (ironclad::index send-output) (- index 8192))
                     (setf (ironclad::buffer send-output)
                           (if (> index 8192)
                               (subseq buffer 8192 index)
                               buffer))
                     (let ((plain-text (if (> index 8192)
                                           (subseq buffer 0 8192)
                                           buffer))
                           (cipher-text (make-array 8192 :element-type '(unsigned-byte 8))))
                       (ironclad:encrypt cipher plain-text cipher-text)
                       cipher-text)))))
    #|

 (defun get-some-stream-octets (stream size) 
    (let ((buffer (ironclad::buffer stream)))
          (index (ironclad::index stream))) 
      (setf (ironclad::index stream) (- index size)) 
      (setf (ironclad::buffer stream) (subseq buffer size index))
      (subseq buffer 0 size))
|#

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

(defun btrfs/send (&key (snapshot-path *path*))
  (ensure-sanity)
  (let ((command (format nil "~A send ~A" *btrfs-command* snapshot-path)))
    (handler-case
        (uiop/run-program::%run-program command :wait nil)
      (t (error)
        (note "btfs send failed on cause ~a." error)
        (return-from btrfs/send nil)))))

(defun btrfs/send-sbcl (&key (snapshot-path *path*))
  (ensure-sanity)
  (let ((command *btrfs-command*)
        (args `("send" ,(namestring snapshot-path))))
    (handler-case
        (sb-ext:run-program command args :wait nil :output :stream)
      (t (error)
        (note "btfs send failed on cause ~a." error)
        (return-from btrfs/send-sbcl nil)))))

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
        
(defun get-cipher ()
  ;;; TODO: initialize cipher correctly
  (ironclad:make-cipher :aes
                        :key
                        (make-array 16 :element-type '(unsigned-byte 8))
                        :mode :cfb
                        :initialization-vector
                        (make-array 16 :element-type '(unsigned-byte 8))))

