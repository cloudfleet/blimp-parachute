(defpackage :chute
  (:nicknames #:parachute #:cloudfleet-parachute #:cloudfleet-chute)
  (:use :cl)
  (:export
   #:client

   #:metadata
   #:timestamp #:shards #:size #:checksum

   #:transfer-blob/http #:put-file 

   #:note

   #:make-blob #:make-blob/test

   #:*blob-storage-dir*

   #:*blob-uri-path*

   #:strip-double-slash

   #:chute-model

   #:make-new-directory

   #:start-api-server #:stop-api-server #:restart-api-server))

(defpackage :chute/crypt
  (:use :cl :chute)
  (:export
   #:get-cipher #:get-key
   #:decrypt-blob-as-octets
   #:encrypt #:encrypt-from
   #:get-cipher
   #:aes-ctr)) ;; State of AES-CTR with 16 byte window index

(defpackage :chute/config
  (:use :cl :chute)
  (:export
   #:*btrfs-command*
   #:default
   #:*api-port*
   #:*blobs-directory*

   #:uri-base

   #:*random-device*
   #:buffer-size

   #:config
   #:client

   #:with-cloudfleet-config

   #:version  #:path #:api.port #:transfer-method
   #:default-mount))

(defpackage :chute/server
  (:use :cl :hunchentoot)
  (:import-from #:chute
                #:note
                #:*blob-uri-path*)
  (:export
   #:*port*
   #:running-server-p
   #:start-server #:stop-server #:restart-server))

(defpackage :chute/fs
  (:use :cl :chute)
  (:export
   #:snapshots

   #:snapshot
   #:snapshot/info
   #:snapshot/mount

   #:send))

(restas:define-module #:chute/api
  (:use #:cl #:chute))

(defpackage :chute/btrfs
  (:use :cl :chute)
  (:export
   #:snapshots
   #:snapshot-info #:snapshot/info
   #:snapshot/mount
   #:snapshot-directory ;;; Hmmm.  Think about how to remove.
   
   #:send

   #:subvolume/snapshot
   #:subvolume/show
   #:subvolume/find-new))
   
(defpackage :chute/zfs
  (:use :cl :chute))

(defpackage :chute/io.cloudfleet
  (:use :cl :chute)
  (:export
   #:engineroom-domain
   #:engineroom-node
   #:engineroom-key))
