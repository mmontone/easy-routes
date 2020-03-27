(asdf:defsystem #:easy-routes+djula
  :description "easy-routes url generator for Djula templates"
  :author "Mariano Montone <marianomontone@gmail.com>"
  :license "MIT"
  :serial t
  :components ((:file "djula"))
  :depends-on (:easy-routes :djula))
