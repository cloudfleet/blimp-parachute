(in-package :chute)

(defun mocked-p () t)

(defun serialize-mocked-p () t)

(defparameter *path*
  "/opt/cloudfleet/data")
(defparameter *snapshot-base*
  "/opt/cloudfleet/data/.snapshot/")
(defparameter *btrfs-command*
  (asdf:system-relative-pathname :chute "../setup/btrfs"))
(defparameter *keystore*
  "/opt/cloudfleet/data/shared/chute/")
(defparameter *random-device*
  "/dev/urandom") ;; "/dev/random" will block
;; REST Server bast
(defparameter *uri-base*
  "http://localhost:2001/chute/blob/" ;; TEST URI
  #+nil
  "http://slack.net/")
;; SERVER
(defparameter *blob-storage-dir*
  (merge-pathnames "blob/storage/" (user-homedir-pathname)))
;; CLIENT
(defparameter *blobs-directory*
  "/var/tmp/blobs/")

(defun ensure-sanity ()
  (unless (probe-file *snapshot-base*)
    (error "No directory to create snapshots at ~s." *snapshot-base*))
  (unless (probe-file *btrfs-command*)
    (error "No setuid btrfs found at ~s." *btrfs-command*))
  #+abcl
  (probe-file *uri-base*)
  t)





