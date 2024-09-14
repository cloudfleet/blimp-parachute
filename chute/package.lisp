(defpackage chute
  (:nicknames #:parachute #:cloudfleet-parachute #:cloudfleet-chute)
  (:use cl)
  (:export
   #:synk ;; N.b. not fully implemented
   
   #:client

   #:metadata
   #:metadata-version
   #:metadata-prototype
   #:metadata-node
   #:metadata-domain
   #:metadata-timestamp
   #:metadata-shards
   #:metadata-size
   #:metadata-checksum

   #:timestamp-now

   #:transfer-blob/http #:put-file

   #:sink

   #:note

   #:make-blob 

   #:*blob-storage-dir*

   #:*blob-uri-path*

   #:strip-double-slash

   #:chute-model

   #:start-api-server #:stop-api-server #:restart-api-server))

(defpackage chute/crypt
  (:use cl chute)
  (:export
   #:get-cipher #:get-key
   #:decrypt-blob-as-octets
   #:encrypt #:encrypt-from
   #:get-cipher
   #:aes-ctr)) ;; State of AES-CTR with 16 byte window index

(defpackage chute/config
  (:use cl chute)
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

   #:version  #:path #:api.port #:transfer-method #:backing-store
   #:default-mount))

(defpackage chute/server
  (:use cl chute hunchentoot)
  (:import-from #:chute
                #:note
                #:*blob-uri-path*)
  (:export
   #:*port*
   #:running-server-p
   #:start-server #:stop-server #:restart-server))

(defpackage chute/fs
  (:use cl chute)
  (:export
   #:snapshots

   #:snapshot

   #:snapshot/info
   #:snapshot/mount

   #:make-directory

   #:send))

(restas:define-module chute/api
  (:use cl chute))

(defpackage chute/btrfs
  (:use cl chute)
  (:export
   #:snapshots
   #:snapshot-info #:snapshot/info
   #:snapshot/mount
   #:snapshot-directory ;;; Hmmm.  Think about how to remove.
   
   #:send

   #:subvolume/snapshot
   #:subvolume/show
   #:subvolume/find-new))

(defpackage chute/zfs
  (:use cl chute))

(defpackage chute/rsync
  (:use cl chute)
  (:export
   #:sink)) ;;; initial implementation: push via rsync 

(defpackage chute/uri
  (:use cl chute)
  (:export
   #:remote-uri
   #:get-uri))

(defpackage chute/io.cloudfleet
  (:use cl chute)
  (:export
   #:domain #:node #:key))

(defpackage nonce.not.org
  (:use cl chute)
  (:export #:index))

  
