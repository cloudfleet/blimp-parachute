(in-package :chute)
;;;; BTRFS specific code
;;;; (swap in ZFS as an exercise?)

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
       :collect (concatenate 'string
                             (string-right-trim "/" path)
                             "/"
                             (string-trim '(#\Space #\Tab) line))
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
