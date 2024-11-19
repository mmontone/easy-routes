(defpackage easy-routes
  (:use :cl)
  (:export
   ;; Hunchentoot acceptor
   #:routes-acceptor
   #:routes-ssl-acceptor
   #:easy-routes-acceptor
   #:easy-routes-ssl-acceptor
   ;; Routes definition
   #:*routes-mapper*
   #:defroute
   #:route
   #:find-route
   #:genurl
   #:genurl*
   #:redirect
   ;; Decorators
   #:@content-type
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
