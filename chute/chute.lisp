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

(defun snapshot (path)
  (declare (ignore path))
  (let* ((o (make-string-output-stream))
         (timestamp (simple-date-time:|yyyymmddThhmmssZ| (simple-date-time:now)))
         (snapshot (format nil "btrfs subvolume snapshot ~a ~a"
                           *path*
                           (concatenate 'string *snapshot-base* timestamp))))
    (uiop:run-program snapshot :output o)
    (get-output-stream-string o)))
           

(defun find-new (path)
  (let* ((o (make-string-output-stream))
         (find-new (format nil "btrfs subvolume find-new ~a" path)))
    (uiop:run-program find-new :output o)
    (get-output-stream-string o)))

         
  

    
        
