;;;; implementation of access to local filesystem via CL:PATHNAME
(in-package :chute/fs)

(defclass fs (chute-model)
  ((unimplemented)))

(defun snapshot (&key (path (chute/config:path (chute/config:default))) path-provided-p)
  (declare (ignore path-provided-p))
  (error "Need to figure out default for snapshot without local URI")
  #+nil
  (chute/btrfs:subvolume/snapshot :path path))

(defun snapshots (&key (path (chute/config:path (chute/config:default))) path-provided-p)
  (declare (ignore path-provided-p))
  #+nil
  (chute/btrfs:snapshots :path path)
  (list "/tmp/"))

(defun snapshot/info (snapshot-path)
  (chute/btrfs:snapshot/info snapshot-path))

(defun send (snapshot-path)
  "Queue snapshot for replication; initiating asynchronous transfer if possible."
  (chute:sink snapshot-path (chute/uri:remote-uri snapshot-path))) 

;;; deprecated 
(defun snapshot/mount (snapshot-path)
  (chute/btrfs:snapshot/mount snapshot-path))


(defun make-directory ()
  "Make a new temporary directory locally"
  (let* ((var-root
          (ensure-directories-exist chute/config:*blobs-directory*))
         (directory-as-file
          (pathname (cl-fad:open-temporary
                     :template (namestring (merge-pathnames "blob-%" var-root)))))
         (directory-with-file ;; huh ??
          (pathname (namestring (concatenate 'string
                                             (namestring directory-as-file)
                                             "/foo")))))
    (delete-file directory-as-file)
    (ensure-directories-exist directory-with-file)
    (pathname (concatenate 'string (namestring directory-as-file) "/"))))
