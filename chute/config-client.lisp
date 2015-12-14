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
;; REST
(defparameter *scheme* "http")
(defparameter *host* "127.0.0.1")
(defparameter *port* 2001)
(defparameter *blob-uri-path* "/chute/blob/")

;; SERVER
(defparameter *blob-storage-dir*
  (merge-pathnames "blob/storage/" (user-homedir-pathname)))
;; CLIENT
(defparameter *blobs-directory*
  "/var/tmp/blobs/")

(defun buffer-size () 8192)

(defun uri-authority ()
  (format nil "~a:~a" *host* *port*))

(defun uri-base ()
  (format nil "~a://~a~a" *scheme* (uri-authority) *blob-uri-path*))

(defun ensure-sanity ()
  (unless (probe-file *snapshot-base*)
    (error "No directory to create snapshots at ~s." *snapshot-base*))
  (unless (probe-file *btrfs-command*)
    (error "No setuid btrfs found at ~s." *btrfs-command*))
  #+abcl
  (probe-file *uri-base*)
  t)





