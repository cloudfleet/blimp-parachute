#-abcl "Needs the Bear"
(prove:plan 1)
(prove:ok
 (jeannie:read-rdf (asdf:system-relative-pathname
                    :chute "../model/chute.n3")
                   :format :n3)
 "Able to serialize model")
(prove:finalize)


                                                  
