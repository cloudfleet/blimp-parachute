(in-package :cl-user)

(prove:plan 1)
(prove:ok
 (chute/config:with-cloudfleet-config
   t)
 "Testing configuration parsing…")

(prove:plan 1)
(prove:ok
 (chute/config:with-cloudfleet-config
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
 "Creating an encrypted blob locally from a file, then comparing with original…") 
  
