(defpackage easy-routes
  (:use :cl)
  (:export
   ;; Hunchentoot acceptor
   #:routes-acceptor
   #:easy-routes-acceptor
   ;; Routes definition
   #:defroute
   ;; Decorators
   #:@html
   #:@json))
