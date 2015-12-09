(defpackage :chute
  (:use :cl)
  (:export
   #:client

   #:metadata
   #:timestamp #:shards #:size #:checksum

   #:backup
   #:note

   #:transfer-blob #:put-file 

   #:snapshot

   #:make-blob #:make-blob/test
   #:decrypt-blob-as-octets

   #:get-cipher #:get-key

   #:*blob-storage-dir*

   #:encrypt

   #:strip-double-slash

   #:make-new-directory

   #:btrfs-snapshots
   #:btrfs/subvolume/snapshot
   #:btrfs/send
   #:btrfs/subvolume/show
   #:btrfs/subvolume/find-new))

(defpackage :chute.server
  (:use :cl :hunchentoot)
  (:import-from #:chute #:note)
  (:export
   #:running-server-p
   #:start-server #:stop-server #:restart-server))

(defpackage :chute.test
  (:use :cl :chute :chute.server))

