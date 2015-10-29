(in-package :chute)

(defun put-file (file &key (uri "http://localhost:2001/blob/foo"))
  (drakma:http-request uri
                       :method :put
                       :content-type "application/octet-stream"
                       :content file))

