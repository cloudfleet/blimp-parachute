(in-package :chute)

(defmethod transfer ((method (eql :http)) snapshot-path)
  (let ((blob-path (uiop:temporary-directory)))
    ;; serialize encrypted blob to path
    (make-blob snapshot-path blob-path)     
    (cloudfleet/transfer-blob blob-path)))

