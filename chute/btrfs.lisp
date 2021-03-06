(in-package :chute/btrfs)
;;;; BTRFS specific code
;;;; (swap in ZFS as an exercise?)

;;; Currently takes a full snaphot
(defun subvolume/snapshot (&key (path (chute/config:default)))
  (let* ((output (make-string-output-stream))
         (error (make-string-output-stream))
         (timestamp (simple-date-time:|yyyymmddThhmmssZ| (simple-date-time:now)))
         (snapshot-path (format nil "~a~a" (snapshot-directory path) timestamp))
         (snapshot (format nil "~a subvolume snapshot -r ~a ~a"
                           chute/config:*btrfs-command*
                           path snapshot-path)))
    (uiop:run-program snapshot :output output :error error)
    (values
     snapshot-path
     (get-output-stream-string output)
     (get-output-stream-string error))))

(defvar *snapshot-prefix* "/.snapshot/")

(defun snapshot-directory (path)
  "Return the location for accumulating snapshots for PATH"
  (concatenate 'string (string-right-trim "/" path) *snapshot-prefix*))

(defun snapshot/mount (snapshot-path)
  "Given a full SNAPSHOT-PATH return the mount point"
  (subseq snapshot-path 0 (1+ (search *snapshot-prefix* snapshot-path))))

;;; TODO Need command to figure out latest generation
(defun subvolume/find-new (&key
                             (path (chute/config:path (chute/config:default)))
                             (generation 0))
  (let* ((o (make-string-output-stream))
         (find-new (format nil "~a subvolume find-new ~a ~a"
                           chute/config:*btrfs-command*
                           path generation)))
    (uiop:run-program find-new :output o)
    (get-output-stream-string o)))

(defun subvolume/show (&key
                         (path (chute/config:path (chute/config:default))))
  (with-output-to-string (output)
    (with-output-to-string (error)
      (let ((command (format nil "~a subvolume show ~a"
                             chute/config:*btrfs-command*
                             path)))
        (handler-case 
            (uiop:run-program command :output output :error error)
          (t (e)
            (declare (ignore e))
            (return-from subvolume/show (values nil output error)))))
      (values output error))))

(defun snapshots (&key
                    (path (chute/config:path (chute/config:default))))
  "List all available snapshots which exist for PATH."
  (let ((show (subvolume/show :path path)))
    (loop
       :for line :in (cl-ppcre:split "\\n" show)
       :with snapshot-region = nil
       :when snapshot-region
       :collect (concatenate 'string
                             (string-right-trim "/" path)
                             "/"
                             (string-trim '(#\Space #\Tab) line))
       :when (cl-ppcre:scan "Snapshot\\(s\\):" line)
       :do (setf snapshot-region t))))

(defun send (snapshot-path)
  "Returns the stream containing the output of the btrfs/send operation on SNAPSHOT-PATH."
  (let ((command (format nil "~A" chute/config:*btrfs-command*))
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
        (return-from send nil)))))

(defun snapshot-info (path)
  (let ((show (subvolume/show :path path))
        (result (make-hash-table :test 'equal)))
    (loop
       :for line :in (cl-ppcre:split "\\n" show)
       :do (multiple-value-bind (match-p matches)
               (cl-ppcre:scan-to-strings "^\\t([^:]+):\\s+(.+)$" line)
             (when match-p 
               (setf (gethash (aref matches 0) result)
                     (aref matches 1)))))
    result))

(defun snapshot/info (path)
  `(:mount
    ,(chute/btrfs:snapshot/mount path)
    :snapshots-directory
    ,(chute/btrfs:snapshot-directory path)
    :snapshots
    ,(snapshot-info path)))
