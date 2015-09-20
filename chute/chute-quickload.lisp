;;; <https://beta.quicklisp.org/quicklisp.lisp>
#-quicklisp
(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp"
                                       (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))

(dolist (system '(:ironclad :simple-date-time :cl-json))
  (ql:quickload system))

