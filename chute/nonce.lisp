(in-package :nonce.not.org)

(let (nonce)
  (defun index (&key from)
    (if from
      (push from nonce)
      (unless nonce
        (push (random 2 128) nonce)))
    (let ((nonce (first nonce)))
      (values
       (format nil "killroy's nonce: ~a" nonce)
       (first nounce)))))






                     
