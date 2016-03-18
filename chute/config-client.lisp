(in-package :chute)

(defclass config () ())

(defclass client (config)
  ((version
    :initform "2016022600"
    :accessor version)
   (prototype
    :initform '(("lispClass" ."client") ("lispPackage". "chute")))
   (path
    :initform "/opt/cloudfleet/data"
    :accessor path)
   (api.port
    :accessor api.port)))

(defparameter *client-config* nil)
(defun get-client-config (&key (force nil))
  (when (or (not *client-config*)
            force )
    (setf *client-config*
          (with-open-file (config (asdf:system-relative-pathname :chute "../etc/client-config.json"))
            (cl-json:with-decoder-simple-clos-semantics
              (cl-json:decode-json config)))))
  *client-config*)
            
;; (defparameter *path*
;;   "/opt/cloudfleet/data/"
;;   "Mount point of filesystem to create snapshots from.")
;; (defparameter *snapshot-base*
;;   "/opt/cloudfleet/data/.snapshot/"
;;   "Base location to name snapshots.")
(defparameter *btrfs-command*
  (asdf:system-relative-pathname :chute "../setup/btrfs"))
(defparameter *keystore*
  "/opt/cloudfleet/data/shared/chute/")
(defparameter *random-device*
  "/dev/urandom") ;; "/dev/random" will block

;; REST contract with CloudFleet backup server
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
  (let* ((path (path (get-client-config)))
         (snapshot (snapshot-directory path)))
    (unless (probe-file path)
      (error "The path to backup doesn't exist at '~a.'" path))
    (unless (probe-file snapshot)
      (error "The path to create snapshots doesn't exist in ~a." snapshot)))
  (unless (probe-file *btrfs-command*)
    (error "No setuid btrfs found at ~s." *btrfs-command*))
  #+abcl
  (probe-file *uri-base*)
  t)





