(in-package :chute)

(defun note (message-or-format &rest args)
  (format t 
          "~&~a ~a" 
          (hunchentoot:rfc-1123-date)
          (apply 'format 
                 nil
                 message-or-format
                 (if args 
                     args
                     nil))))
