(defpackage :chute
  (:use :cl))

(in-package :chute)

(defun backup ()
  ;;; create snapshot
  ;;; serialize snapshot to directory encrypting blob
  ;;; get backup off system
  )

(defparameter *path* "/opt/cloudfleet/data")
(defparameter *snapshot-base* "/opt/cloudfleet/data/.snapshot/")
(defparameter *btrfs-command* (asdf:system-relative-pathname :chute "../setup/btrfs"))

      

(defun ensure-sanity ()
  (unless (probe-file *snapshot-base*)
    (error "No directory to create snapshots at ~s." *snapshot-base*))
  (unless (probe-file *btrfs-command*)
    (error "No setuid btrfs found at ~s." *btrfs-command*))
  t)

(defun subvolume-snapshot (&key (path *path*))
  (ensure-sanity)
  (let* ((o (make-string-output-stream))
         (timestamp (simple-date-time:|yyyymmddThhmmssZ| (simple-date-time:now)))
         (snapshot (format nil "~a subvolume snapshot -r ~a ~a"
                           *btrfs-command*
                           path
                           (concatenate 'string *snapshot-base* timestamp))))
    (uiop:run-program snapshot :output o)
    (get-output-stream-string o)))

(defun subvolume-find-new (&key (path *path*) generation)
  (ensure-sanity)
  (let* ((o (make-string-output-stream))
         (find-new (format nil "~a subvolume find-new ~a ~a"
                           *btrfs-command*
                           path generation)))
    (uiop:run-program find-new :output o)
    (get-output-stream-string o)))

(defun subvolume-show (&key (path *path*))
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
            (return-from subvolume-show (values nil output error)))))
      (values t output error))))

;;; FIXME:  return something that can be used
(defun send (&key (path *path*))
  (ensure-sanity)
  (with-output-to-string (output) ;; this will be closed
    (with-output-to-string (error)
      (let ((command (format nil "~a send ~a | cat"
                             *btrfs-command*
                             path)))
        (handler-case 
            (uiop:run-program command :output output :error error)
          (t (e)
            (return-from send (values nil output (get-output-stream-string error) e)))))
      (values t (get-output-stream-string output) (get-output-stream-string error)))))



         
  

    
        
