(in-package :chute)

(defun mocked-p ()
  t)

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

;;; TODO Need command to figure out latest generation
(defun btrfs/subvolume/find-new (&key (path *path*) (generation 0))
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
      (values output error))))

(defun btrfs-snapshots (&key (path *path*))
  "List all available snapshots which exist for PATH."
  (let ((show (btrfs/subvolume/show :path path)))
    (loop
       :for line :in (cl-ppcre:split "\\n" show)
       :with snapshot-region = nil
       :when snapshot-region
       :collect (concatenate 'string path "/" (string-trim '(#\Space #\Tab) line))
       :when (cl-ppcre:scan "Snapshot\\(s\\):" line)
       :do (setf snapshot-region t))))

(defun btrfs/send (snapshot-path)
  "Returns the stream containing the output of the btrfs/send operation on SNAPSHOT-PATH."
  (ensure-sanity)
  (let ((command (format nil "~A" *btrfs-command*))
        (args (list "send" snapshot-path)))
    (handler-case
        (let ((result
                #+sbcl
                (sb-ext:run-program command args :wait nil
                                    :output :stream
                                    :error :stream)
;;                                    :external-format :iso-8859-1) ;; FIXME: how to get octets
                #+ccl
                (ccl:run-program command args :wait nil
                                 :output :stream
                                 :error :stream
                                 :element-type '(unsigned-byte 8))
                #-(or sbcl ccl) ;; Untested, probably broken
                (uiop/run-program::%run-program (format nil "~a ~a" command args) :wait nil
                                                :output :stream
                                                :input :stream)))

          #+sbcl
          (values (slot-value result 'sb-impl::output)
                  (slot-value result 'sb-impl::error))
          #+ccl
          (values (slot-value result 'ccl::output)
                  (slot-value result 'ccl::error))
          #-(or sbcl ccl) ;; Broken
          (values result))
      (t (error)
        (note "btrfs send failed with '~a'." error)
        (return-from btrfs/send nil)))))
