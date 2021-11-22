
(defpackage #:clorg-demo
  (:use #:clim #:clim-lisp #:clim-extensions)
  (:export #:*eu-employees*
           #:*eu-org-chart*))

(in-package #:clorg-demo)

(defparameter *eu-employees*
  '((:name "Ursula Von Der Leyen" :role "President of The European Comission"
     :reports ((:name "Vera Jourova"
                      :role "Vice President"
                      :subrole "Values and Transparency"
                      :reports ((:name "Dider Reynders"
                                       :role "Comissioner"
                                       :subrole "Justice")))
               ;; add an extra "reports" layer to get this to drop down one level
               (:reports
                ((:name "Maro Sefcovic"
                        :role "Vice President")))
               (:name "Frans Timmerman"
                      :role "First Executive Vice President")
               (:name "Margaritis Schinas"
                      :role "Vice President"
                      :reports ((:name "Mariya Gabriel"
                                       :role "Commissioner")))
               (:name "Margrethe Vesteger"
                      :role "Executive Vice President")
              (:name "Valdis Dombrovskis"
                      :role "Executive Vice President")))))

(defparameter *eu-org-chart* (clorg:make-org-chart *eu-employees*))

