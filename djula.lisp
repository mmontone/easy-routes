;; generate easy-route routes urls from djula templates
;; Syntax: {% genurl route-name &rest args %}
;; Example: {% genurl my-route :id 22 :foo template-var.key %}

(in-package :easy-routes)

(djula:def-tag-compiler genurl (route &rest args)
  (lambda (stream)
    (let ((args (loop
                  for key in args by #'cddr
                  for value in (rest args) by #'cddr
                  collect (intern (symbol-name key) :keyword)
                  collect (cond
                            ((stringp value) value)
                            ((numberp value) value)
                            (t (djula::resolve-variable-phrase (djula::parse-variable-phrase (princ-to-string value))))))))
      (write-string (apply #'genurl
                           (intern (symbol-name route) djula::*djula-execute-package*)
                           args)
                    stream))))
