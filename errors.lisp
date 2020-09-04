(in-package :easy-routes)

(defclass easy-routes-errors-acceptor (hunchentoot-errors:errors-acceptor)
  ())

(defmethod hunchentoot-errors::acceptor-log-error
    (stream (acceptor easy-routes-errors-acceptor) log-level format-string &rest format-arguments)
  (declare (ignore format-arguments))
  (call-next-method)
  (let ((route (and (boundp 'hunchentoot:*request*)
                    (routes:match (acceptor-routes-mapper (hunchentoot:acceptor-name acceptor))
                      (hunchentoot:request-uri*)))))
    (when route
      (format stream "ROUTE: ")
      (print-route route stream)
      (terpri stream))))

(defmethod hunchentoot:acceptor-status-message ((acceptor easy-routes-errors-acceptor) http-status-code &key &allow-other-keys)
  (concatenate
   'string
   (call-next-method)
   (if hunchentoot:*show-lisp-errors-p*
       (let ((route (and (boundp 'hunchentoot:*request*)
                         (routes:match (acceptor-routes-mapper (hunchentoot:acceptor-name acceptor))
                           (hunchentoot:request-uri*)))))
         (when route
           (with-output-to-string (msg)
             (format msg "<h1>Route</h1>~%")
             (format msg "<p>")
             (print-route route msg)
             (format msg "</p>"))))
    "")))
