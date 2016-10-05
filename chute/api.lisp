(in-package :chute/api)

(restas:define-route %index ("")
  (who:with-html-output-to-string (o)
    (:html
     (:body
      (:h1 "Parachute API")
      (:ul
       (:li
        (:a :href (restas:genurl '%snapshot)
            (who:str "List Snapshots"))))))))

(restas:define-route %snapshot ("/snapshot")
  (who:with-html-output-to-string (o)
    (:html
     (:body
      (:h1 "Snapshot")
      (:div :id "snapshot-create"
            (:h2 "Create")
            (:form :method :post
                   (:input :id "snapshot-create-submit" :type "submit" :value "Take new snapshot")))
      (:div :id "snapshot-list"
            (:h2 "Existing Snapshots")
            (:ul
             (loop :for snapshot :in (chute/fs:snapshots)
                :do (who:htm (:li :class "snapshot-list-item" (who:str snapshot))))))))))

(restas:define-route %snapshot.post ("/snapshot" :method :post)
  (chute/fs:snapshot)
  (restas:redirect '%snapshot))

  
  


      
