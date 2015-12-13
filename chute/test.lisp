(in-package :chute.test)

;;; Working on blimp if BTRFS is present
(rt:deftest blob.from-snapshop.1
    (let ((snapshots (btrfs-snapshots)))
      (unless snapshots
        (error "No snapshots to send."))
      (make-blob
       (first snapshots)
       #p"/var/tmp/blob/"))
  t)

;;; lowlevel test of btrfs send snapshot to stream
(rt:deftest btrfs.send-snapshot.1
    ;;; assuming there is at least one snapshot
    (let ((snapshots (btrfs-snapshots)))
      (unless snapshots
        (error "No snapshots to send."))
      (let ((result (btrfs/send (first (btrfs-snapshots)))))
        (and
         (streamp result)
         (equal (stream-element-type result) '(unsigned-byte 8)))))
t)

;;; create an encrypted blob locally from a file, then compare with original
(rt:deftest blob.from-file.1
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
                          nil))))
  t)
   
;;; Demonstrate that IRONCLAD AES block ciphers indeed retain state
;;; TODO fix test so that it actually compares its values
(rt:deftest aes.block-state.1
    (let ((cipher (get-cipher :aes))
          (cipher2 (get-cipher :aes))
          (plain-1 (ironclad:ascii-string-to-byte-array "this"))
          (cipher-1 (make-array 4 :element-type '(unsigned-byte 8)))
          (plain-2 (ironclad:ascii-string-to-byte-array "open"))
          (cipher-2 (make-array 4 :element-type '(unsigned-byte 8))))
    (ironclad:encrypt cipher plain-1 cipher-1)
    (ironclad:encrypt cipher plain-2 cipher-2)
    (let ((cipher-12 (make-array 8 :element-type '(unsigned-byte 8)))
          (plain-12 (make-array 8 :element-type '(unsigned-byte 8))))
      (replace cipher-12 cipher-1 :start1 0 :end1 4 :start2 0)
      (replace cipher-12 cipher-2 :start1 4 :end1 8 :start2 0)
      (ironclad:decrypt cipher2 cipher-12 plain-12)
      (values cipher plain-1 cipher-1 plain-2 cipher-2 cipher-12 plain-12)))
    t)

(rt:deftest blob.transfer.1
    (let ((directory (make-blob #p"/etc/passwd" (make-new-directory)))
          (already-running-server-p (chute.server:running-server-p)))
      (unless already-running-server-p
        (chute.server:start-server))
      (prog1
          (multiple-value-list
           (transfer-blob directory))
        (unless already-running-server-p
          (chute.server:stop-server))))
  t)





