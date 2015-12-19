(in-package :chute)

(defun engineroom-domain ()
  (some (lambda (value) value)
        `(,(let ((output (make-string-output-stream)))
                (uiop:run-program "cat /opt/cloudfleet/data/config/domain.txt" :output output)
                (get-output-stream-string output))
         "example.com")))

(defun engineroom-node ()
  "urn:chute:node:0")

;;; XXX How to get key if USB cf-key has been unmounted
;;; XXX how to read key when owned by root
(defun engineroom-key ()
  (let ((key-file #p"/mnt/storage-key/key"))
    (unless (probe-file key-file)
      (warn "No key file found at '~a'." key-file)
      (return-from engineroom-key nil))
    (handler-case 
        (ironclad:digest-file :sha256 key-file)
      (file-error (c)
        (warn "Failed to read key from '~a' because '~a'"
              key-file c)
        (return-from engineroom-key nil)))))
      
  
    


