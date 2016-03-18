;;; <https://beta.quicklisp.org/quicklisp.lisp>
#-quicklisp
(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp"
                                       (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))

(dolist (system '(:ironclad
                  :lparallel
                  :simple-date-time :cl-json
                  :hunchentoot
                  :restas :cl-who
                  :drakma
                  :osicat
                  :rt))
  (ql:quickload system))

