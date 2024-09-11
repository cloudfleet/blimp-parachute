(in-package chute/uri)

;;; very ABCL specific in the ability to construct a remote URI addressable via CL:OPEN
#+abcl 
(progn 
  (defun remote-uri (local)
    (make-pathname :host "rdf.not.org"
                   :scheme "https"
                   :defaults (pathname local)))
  (defun get-uri (uri)
    (uiop:copy-file
     (open uri :direction :input)
     destination)))
#-abcl "Need the Bear here."

                 
