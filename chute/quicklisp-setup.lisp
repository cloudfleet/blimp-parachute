;;; <https://beta.quicklisp.org/quicklisp.lisp>
#-quicklisp
(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp"
                                       (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))

(dolist (system '(:ironclad
                  :lparallel
                  :cl-date-time-parser
                  :simple-date-time :cl-json
                  :hunchentoot
                  :restas :cl-who
                  :drakma
                  #+nil 
                  :osicat ;; To use for shared memory access, currently unused
                  :rt))
  (ql:quickload system))

