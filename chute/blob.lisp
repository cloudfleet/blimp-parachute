(in-package :chute)

(defclass metadata ()
  ((version
    :initform "2015102901"
    :documentation "Version of blob metadata.")
   (node
    :initform "urn:chute:node:0"
    :documentation "Node creating this blob.")
   (domain
    :initform "example.com"
    :documentation "Domain creating this blob.")
   (mount
    :initform *path*
    :documentation "Mount point of blob.")
   (timestamp
    :reader timestamp
    :initform (simple-date-time:rfc-2822 (simple-date-time:now)))
   (parent
    :initform nil
    :documentation "Previous blob, or nil if this is the first blob in a series.")
   (shards
    :initform 1
    :accessor shards
    :documentation "Number of pieces (shards) the blob is split across.")
   (size
    :accessor size
    :documentation "Size of blob in bytes.")
   (checksum
    :documentation "Checksum of blob.")))

