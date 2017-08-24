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
   #:http-error
   #:not-found-error
   #:permission-denied-error
   #:or-http-error
   #:or-not-found))
