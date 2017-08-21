(defpackage easy-routes
  (:use :cl)
  (:export
   ;; Hunchentoot acceptor
   #:routes-acceptor
   #:easy-routes-acceptor
   ;; Routes definition
   #:defroute
   #:route
   #:find-route
   #:genurl
   #:genurl*
   #:redirect
   ;; Decorators
   #:@html
   #:@json
   ;; HTTP errors
   #:or-http-error
   #:or-http-not-found))
