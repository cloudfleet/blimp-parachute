(in-package :chute)

@route PUT "/blob/:timestamp/:hash/:n-bytes/:mth-window"
(defun put-byte-range (&key timestamp hash n-bytes mth-window)
  ;;; Locate output directory
  ;;; Get a writable stream to the object
  ;;; Slurp bytes, writing to disk
  (warn "Unimplemented PUT-BYTE-RANGE for ~a/~a." timestamp hash))

