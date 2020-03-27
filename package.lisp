(defpackage easy-routes
  (:use :cl)
  (:export
   ;; Hunchentoot acceptor
   #:routes-acceptor
   #:easy-routes-acceptor
   ;; Routes definition
   #:*routes-mapper*
   #:defroute
   #:route
   #:find-route
   #:genurl
   #:genurl*
   #:redirect
   ;; Decorators
   #:@html
   #:@json
   #:@check
   #:@check-permission
   ;; HTTP errors
   #:http-error
   #:not-found-error
   #:permission-denied-error
   #:or-http-error
   #:or-not-found))
