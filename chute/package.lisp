(defpackage :chute
  (:use :cl)
  (:export

   #:metadata
   #:timestamp #:shards #:size

   #:backup
   #:note

   #:put-file

   #:snapshot #:snapshot/mock

   #:make-blob #:make-blob/mock

   #:get-cipher

   #:encrypt

   #:btrfs/subvolume/snapshot
   #:btrfs/send
   #:btrfs/subvolume/find-new))

(defpackage :chute.server
  (:use :cl :hunchentoot)
  (:export
   #:running-server-p
   #:start-server #:stop-server #:restart-server))

(defpackage :chute.test
  (:use :cl :chute :chute.server))

