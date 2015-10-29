(defpackage :chute
  (:use :cl)
  (:export

   #:metadata
   #:timestamp #:shards #:size

   #:backup

   #:btrfs/subvolume/snapshot
   #:btrfs/send
   #:btrfs/subvolume/find-new))

(defpackage :chute.test
  (:use :cl :chute))

(defpackage :chute.server
  (:use :cl :hunchentoot)
  (:export

   #:start-server))


