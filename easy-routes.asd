(asdf:defsystem #:easy-routes
  :description "Yet another routes handling utility on top of Hunchentoot"
  :author "Mariano Montone <marianomontone@gmail.com>"
  :license "MIT"
  :homepage "https://github.com/mmontone/easy-routes"
  :long-description
  #.(uiop:read-file-string
     (uiop:subpathname *load-pathname* "README.md"))
  :serial t
  :components ((:file "package")
               (:file "util")
               (:file "compute-parameter")
               (:file "easy-routes")
               (:file "routes-map-printer"))
  :depends-on (:hunchentoot :routes))
