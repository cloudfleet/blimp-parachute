(load (merge-pathnames "quicklisp.lisp"
                       (user-homedir-pathname)))

(funcall (intern (symbol-name 'install) :quicklisp-quickstart))

(with-open-file (rc-init (merge-pathnames
                           #+ccl
                           ".ccl-init.lisp"
                           #+abcl
                           ".abclrc"
                           #+sbcl
                           ".sbclrc"
                           #+ecl
                           ".eclrc"
                           (user-homedir-pathname))
                          :direction :output
                          :if-exists :supersede)
  (write '(progn
           (setf *load-verbose* t)
           (require :asdf)
           (let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp"
                                                  (user-homedir-pathname))))
             (when (probe-file quicklisp-init)
               (load quicklisp-init))))
           :stream rc-init))
;;; TODO parse variables in <file:../etc/cf-vars.sh>
(require :asdf)
(load (asdf:system-relative-pathname (asdf:find-system :chute) "quicklisp-setup.lisp"))

(quit)
