
(asdf:defsystem #:clorg
  :depends-on (#:mcclim #:graph #:with-assoc)
  :serial t
  :components
  ((:file "package")
   (:file "org-chart")
   (:file "demo-org-chart")
   (:file "clorg")))
