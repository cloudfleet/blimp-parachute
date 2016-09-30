(in-package :chute/test)

(defmacro with-cloudfleet-config (&body body)
  `(progn
     (get-client-config :file "client-config.json" :force t)
     ,@body))

;;; Working on blimp if BTRFS is present
(rt:deftest btrfs.snapshot.make-blob.1
    (with-cloudfleet-config
      (let ((snapshots (btrfs-snapshots)))
        (unless snapshots
          (error "No snapshots to send."))
        (make-blob
         (first (reverse snapshots))
         (make-new-directory))))
  t)

;;; lowlevel test of btrfs send snapshot to stream
(rt:deftest btrfs.send-snapshot.1
    (with-cloudfleet-config
    ;;; assuming there is at least one snapshot
      (let ((snapshots (btrfs-snapshots)))
        (unless snapshots
          (error "No snapshots to send."))
        (let ((result (btrfs/send (first (btrfs-snapshots)))))
          (and
           (streamp result)
           (equal (stream-element-type result) '(unsigned-byte 8))))))
t)

;;; create an encrypted blob locally from a file, then compare with original
(rt:deftest make-blob.from-file.1
    (with-cloudfleet-config
      (let* ((file #p"/etc/passwd")
             (blob-directory (make-blob file (make-new-directory)))
             (octets (decrypt-blob-as-octets blob-directory)))
        (with-open-file (stream file :direction :input :element-type '(unsigned-byte 8))
          (loop
             :for n :from 0
             :with byte 
             :do (setf byte (read-byte stream nil))
             :when (null byte) ;; EOF
             :return t
             :unless (= byte (elt octets n))
             :return (progn (note "Mismatch at byte ~a between ~a and blob in ~a.~&Decrypted octets:~%~a~%Original octets:~%~a~%"
                                  n file blob-directory octets (flexi-streams:octets-to-string octets))
                            nil)))))
  t)

;;;  Simple test that AES block ciphers indeed retain state
;;;    D( E(x1) || E(x2) ) == x1 || x2
(rt:deftest aes.block-state.1
    (let ((key-1 (get-cipher :aes-ctr))
          (key-2 (get-cipher :aes-ctr))
          (plain-1 (ironclad:ascii-string-to-byte-array "this"))
          (cipher-1 (make-array 4 :element-type '(unsigned-byte 8)))
          (plain-2 (ironclad:ascii-string-to-byte-array "open"))
          (cipher-2 (make-array 4 :element-type '(unsigned-byte 8)))
          (plain-12 (make-array 8 :element-type '(unsigned-byte 8))))
      (ironclad:encrypt key-1 plain-1 cipher-1)
      (ironclad:encrypt key-1 plain-2 cipher-2)
      (ironclad:decrypt key-2
                        (concatenate '(vector (unsigned-byte 8) *) cipher-1 cipher-2)
                        plain-12)
      (values
       (= (+ (length plain-1) (length plain-2))
          (length plain-12))
       (loop
          :for i :across (concatenate '(vector (unsigned-byte 8) *) plain-1 plain-2)
          :for j :across plain-12
          :unless (eq i j)
          :return nil
          :finally (return t))))
    t t)

(rt:deftest blob.http.transfer.1
    (let ((directory (make-blob #p"/etc/passwd" (make-new-directory)))
          (already-running-server-p (chute/server:running-server-p)))
      (unless already-running-server-p
        (chute/server:start-server))
      (prog1
          (let ((results
                 (multiple-value-list
                  (transfer-blob/http directory))))
            ;;; TODO add other checks for successful transfer
            (= (second results) 201))
        (unless already-running-server-p
          (chute/server:stop-server))))
  t)





