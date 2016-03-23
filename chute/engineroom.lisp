(in-package :chute)

(defun engineroom-domain ()
  "Return the dns domain for the node as configured in cloudfleet/engineroom"
  (some (lambda (value) value)
        `(,(let ((output (make-string-output-stream)))
                (uiop:run-program "cat /opt/cloudfleet/data/config/domain.txt" :output output)
                (get-output-stream-string output))
         "example.com")))

(defun engineroom-node ()
  ;;; A dummy placeholder for now, as all domains only have a single node.
  "urn:chute:node:0")

;;; currently depends on the 'enable-backup' branch of cloudfleet/engineroom
(defun engineroom-key ()
  "Return an aes key derived from the CloudFleet engineroom storage key

Returns nil if the aes key cannot be derived for some reason."
  (let ((key-file #p"/opt/cloudfleet/data/shared/crypt/storage-key"))
    (unless (probe-file key-file)
      (warn "No key file found at '~a'." key-file)
      (return-from engineroom-key nil))
    (handler-case 
        (ironclad:digest-file :sha256 key-file)
      (file-error (c)
        (warn "Failed to read key from '~a' because '~a'"
              key-file c)
        (return-from engineroom-key nil)))))
      
  
    


