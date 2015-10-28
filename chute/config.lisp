(in-package :chute)

(defparameter *path*
  "/opt/cloudfleet/data")
(defparameter *snapshot-base*
  "/opt/cloudfleet/data/.snapshot/")
(defparameter *btrfs-command*
  (asdf:system-relative-pathname :chute "../setup/btrfs"))
(defparameter *keystore*
  "/opt/cloudfleet/data/shared/chute/")

(defparameter *uri-base*
  "http://localhost:2001/blob"
#+nil
  "http://slack.net/")



