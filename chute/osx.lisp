;;; Tests for OSX development
(in-package :chute)

(defun osx/config ()
  (setf *blobs-directory* "/var/tmp/blobs/"))

;;; Make a test bob

(defun osx/make-test-blob ()
  (osx/config)
  (uiop/run-program:run-program "dd if=/dev/zero of=/tmp/xx bs=1m count=10" :output t :error-output t)
  (make-blob #p"/tmp/xx" (make-new-directory)))
