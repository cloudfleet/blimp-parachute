(in-package :chute)

(defun client (&key (config "client-config.json"))
  "Boot the chute client computation locally."
  (note "Starting client configured via ~a" config)
  (chute/config:default :file config :force t)
  (start-api-server)
  (if lparallel:*kernel*
      (warn "lparallel kernel unexpectedly present.")
      (setf lparallel:*kernel* (lparallel:make-kernel 3)))
  (let ((channel (lparallel:make-channel)))
    (values
     channel
     (lparallel:submit-task channel
                            #'snapshot-task) 
     (lparallel:submit-task channel
                            #'transfer-task))))

(defun synk (local remote) ;; mebbe add a "window" or "epoch" hint here
  "Whole kitchen sync for local and remote state"
  (some 
   (lambda () (chute/rsync:sink local remote))
   (error "Unimplemented creation of sink")))

(defun node ()
  (values 0 
          #p"https://waste.not.org/~a/"))



