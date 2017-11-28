(in-package chute/config)

(defparameter *client-config* nil)

(defclass config ()
  ())

(defclass client (config)
  ((version
    :initform "2016022600"
    :accessor version)
   (prototype
    :initform '(("lispClass" ."client") ("lispPackage". "chute")))
   (path
    :documentation "Location of data for client."
    :initform (or (probe-file #p"/opt/io/cloudfleet/data")
                  #+abcl
                  (probe-file #p"https://api.cloudfleet.io/client/config"))
    :accessor path)
   (backing-store
    :documentation "Locally available filesystem abstractions for snapshoting."
    :initform (alexandria:random-elt '(:btrfs :zfs :rsync)))
   (api.port
    :accessor api.port
    :initform 2020)
   (transfer-method
    :accessor transfer-method)))

(defun default (&key (file "client-config.json") (force nil))
  (when (or (not *client-config*)
            force )
    (setf *client-config*
          (with-open-file (config (asdf:system-relative-pathname :chute
                                                                 (format nil "../etc/~a" file)))
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
(defparameter *tmp-directory* "/mnt/storage/tmp/")
(defparameter *blobs-directory* (merge-pathnames "blobs/" *tmp-directory*))

(defun buffer-size () 8192)

(defun uri-authority ()
  (format nil "~a:~a" *host* *port*))

(defun uri-base ()
  (format nil "~a://~a~a" *scheme* (uri-authority) *blob-uri-path*))

(defmacro with-cloudfleet-config (&body body)
  `(progn
     (chute/config:default :file "client-config.json" :force t)
     ,@body))

#+nil
(defun ensure-sanity ()
  (let* ((path (chute/config:path (chute/config:default)))
         (snapshot (assoc :snapshots-directory (chute/fs:snapshot/info path))))
    (unless (probe-file path)
      (error "The path to backup doesn't exist at '~a.'" path))
    (unless (probe-file snapshot)
      (error "The path to create snapshots doesn't exist in ~a." snapshot)))
  (unless (probe-file *btrfs-command*)
    (error "No setuid btrfs found at ~s." *btrfs-command*))
  #+abcl
  (probe-file *uri-base*)
  t)





