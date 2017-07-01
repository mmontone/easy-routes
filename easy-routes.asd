(asdf:defsystem #:easy-routes
  :description "Yet another routes handling utility on top of Hunchentoot"
  :author "Mariano Montone <marianomontone@gmail.com>"
  :license "MIT"
  :serial t
  :components ((:file "package")
               (:file "util")
               (:file "easy-routes"))
  :depends-on (:hunchentoot :routes))
