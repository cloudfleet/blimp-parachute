(in-package :cl-user)

(prove:plan 1)
(prove:ok
 (make-instance 'chute:metadata)
 "Able to create unpopulated metadata object.")

(prove:finalize)
