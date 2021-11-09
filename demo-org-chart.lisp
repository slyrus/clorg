
(in-package #:clorg)

(defparameter *employees*
  '((:name "The Top Boss" :role "CEO"
     :reports ((:name "Joe Strummer" :role "Chief Clash Officer"
                      :reports ((:name "Mick Jones" :role "Jonesy / Hooks")
                                (:name "Paul Simonon" :role "Bass and Occasional Vocals")
                                (:name "Topper Headon" :role "Skins")))
               (:name "Keith Richards" :role "Chief Guitaristic Officer"
                      :reports ((:name "Mick Jagger" :role "Singer / Dancer"
                                       :reports ((:name "Charlie Watts" :role "RIP")
                                                 (:name "Darryl" :role "Bass")))))
               (:name "Sting" :role "The Face")
               ;; add an extra "reports" layer to get this to drop down one level
               (:reports
                ((:name "Ray Davies" :role "Vocals and Guitar")))))))

(defparameter *org-chart* (make-org-chart *employees*))

