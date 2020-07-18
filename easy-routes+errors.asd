(asdf:defsystem #:easy-routes+errors
  :description "Better error pages and logs for easy-routes"
  :author "Mariano Montone <marianomontone@gmail.com>"
  :license "MIT"
  :serial t
  :components ((:file "errors"))
  :depends-on (:easy-routes :hunchentoot-errors))
