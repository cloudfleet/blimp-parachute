(in-package :chute/fs)

(defclass fs (chute-model)
  ((unimplemented)))

(defun snapshot (&key (path (chute/config:path (chute/config:default))) path-provided-p)
  (declare (ignore path-provided-p))
  (chute/btrfs:subvolume/snapshot :path path))

(defun snapshots (&key (path (chute/config:path (chute/config:default))) path-provided-p)
  (declare (ignore path-provided-p))
  (chute/btrfs:snapshots :path path))

(defun send (snapshot-path)
  "Queue snapshot for replication; initiating asynchronous transfer if possible."
  (chute/btrfs:send snapshot-path))

(defun snapshot/mount (snapshot-path)
  (chute/btrfs:snapshot/mount snapshot-path))

(defun snapshot/info (snapshot-path)
  (chute/btrfs:snapshot/info snapshot-path))
