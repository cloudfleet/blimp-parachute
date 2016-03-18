(in-package :api)

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
      (:div :id "create"
            (:h2 "Create")
            (:form :method :post
                   (:input :type "submit" :value "Take new snapshot")))
      (:div :id "existing"
            (:h2 "Existing Snapshots")
            (:ul
             (loop :for snapshot :in (btrfs-snapshots)
                :do (who:htm (:li (who:str snapshot))))))))))

(restas:define-route %snapshot.post ("/snapshot" :method :post)
  (btrfs/subvolume/snapshot)
  (restas:redirect '%snapshot))

  
  


      
