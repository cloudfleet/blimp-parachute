;;; Tests for OSX development
(in-package :chute/config)

(defun osx/config ()
   (setf chute/config:*blobs-directory* "/var/tmp/blobs/"))

(in-package :chute)
;;; Make a test bob
(defun osx/make-test-blob ()
  (chute/config::osx/config)
  (uiop/run-program:run-program "dd if=/dev/zero of=/tmp/xx bs=1m count=10" :output t :error-output t)
  (make-blob #p"/tmp/xx" (make-new-directory)))
