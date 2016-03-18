(defpackage :chute
  (:use :cl)
  (:export
   #:client

   #:metadata
   #:timestamp #:shards #:size #:checksum

   #:transfer-blob #:put-file 

   #:note

   #:make-blob #:make-blob/test
   #:decrypt-blob-as-octets

   #:get-cipher #:get-key

   #:*blob-storage-dir*

   #:*blob-uri-path* #:*port*

   #:encrypt

   #:strip-double-slash

   #:make-new-directory

   #:aes-ctr ;; State of AES-CTR with 16 byte window index

   #:start-api-server #:stop-api-server #:restart-api-server

   #:get-client-config

   #:btrfs-snapshots
   #:btrfs/subvolume/snapshot
   #:btrfs/send
   #:btrfs/subvolume/show
   #:btrfs/subvolume/find-new))

(defpackage :chute.server
  (:use :cl :hunchentoot)
  (:import-from #:chute
                #:note
                #:*port*
                #:*blob-uri-path*)
  (:export
   #:running-server-p
   #:start-server #:stop-server #:restart-server))

(defpackage :chute.test
  (:use :cl :chute :chute.server))

(restas:define-module #:chute.api
  (:use #:cl #:chute)
  (:nicknames #:api))


