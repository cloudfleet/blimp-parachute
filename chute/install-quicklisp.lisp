(load (merge-pathnames "quicklisp.lisp"
                       (user-homedir-pathname)))
(with-open-file (ccl-init (merge-pathnames ".ccl-init.lisp"
                                           (user-homedir-pathname))
                          :direction :output
                          :if-exists :supersede)
  (write ccl-init
         '((let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp"
                                       (user-homedir-pathname))))
             (when (probe-file quicklisp-init)
               (load quicklisp-init)))
           (setf *load-verbose* t))))

(load "/opt/cloudfleet/app/chute/quicklisp-setup.lisp")

(quit)
