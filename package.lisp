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
   #:@header-out
   #:@headers-out
   #:@accept
   #:@html
   #:@json
   #:@check
   #:@check-permission
   #:@cors
   ;; HTTP errors
   #:http-error
   #:not-found-error
   #:permission-denied-error
   #:or-http-error
   #:or-not-found))
