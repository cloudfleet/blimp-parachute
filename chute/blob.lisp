(in-package :chute)

(defclass blob-metadata ()
  ((version
    :initform "2015092401"
    :documentation "Version of blob metadata.")
   (node
    :documentation "Node creating this blob.")
   (domain
    :documentation "Domain creating this blob.")
   (mount
    :initform *path*
    :documentation "Mount point of blob.")
   (timestamp
    :reader timestamp
    :initform (simple-date-time:rfc-2822 (simple-date-time:now)))
   (parent 
    :documentation "Previous blob, or nil if this is the first blob in a series.")
   (checksum
    :documentation "Checksum of blob.")))

