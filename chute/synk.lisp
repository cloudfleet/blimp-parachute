(in-package :chute)

(defun synk (local remote) ;; mebbe add a "window" or "epoch" hint here
  "Whole kitchen sync for local and remote state"
  (lambda () (chute/rsync:sink local remote)))
