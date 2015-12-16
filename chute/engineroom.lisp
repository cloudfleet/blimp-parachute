(in-package :chute)

(defun engineroom-domain ()
  (some (lambda (value) value)
        `(,(let ((output (make-string-output-stream)))
                (uiop:run-program "cat /opt/cloudfleet/data/config/domain.txt" :output output)
                (get-output-stream-string output))
         "example.com")))

(defun engineroom-node ()
  "urn:chute:node:0")


