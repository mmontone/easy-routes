(in-package :easy-routes)

(defparameter *routes* (make-hash-table))
(defparameter *routes-mapper* (make-instance 'routes:mapper)
  "This is the cl-routes map of routes.
cl-routes implements special SWANK inspect code and prints routes mappers as a tree.
Just inspect *routes-mapper* from the Lisp listener to see.")

(defparameter *acceptors-routes-and-mappers* (make-hash-table)
  "Routes and route mappers for individual acceptors")

(defvar *route* nil "The current route")

(defclass routes-acceptor (hunchentoot:acceptor)
  ()
  (:documentation "This acceptors handles routes and only routes. If no route is matched then an HTTP NOT FOUND error is returned.
If you want to use Hunchentoot easy-handlers dispatch as a fallback, use EASY-ROUTES-ACCEPTOR"))

(defun acceptor-routes-mapper (acceptor-name)
  (or (getf (gethash acceptor-name *acceptors-routes-and-mappers*)
            :routes-mapper)
      *routes-mapper*))

(defun acceptor-routes (acceptor-name)
  (or (getf (gethash acceptor-name *acceptors-routes-and-mappers*)
            :routes)
      *routes*))

(defun ensure-acceptor-routes-and-mapper (acceptor-name)
  (or (gethash acceptor-name *acceptors-routes-and-mappers*)
      (setf (gethash acceptor-name *acceptors-routes-and-mappers*)
            (list :routes (make-hash-table) :routes-mapper (make-instance 'routes:mapper)))))

(defmethod hunchentoot:acceptor-dispatch-request
    ((acceptor routes-acceptor) request)
  (flet ((not-found-if-null (thing)
           (unless thing
             (setf (hunchentoot:return-code*)
                   hunchentoot:+http-not-found+)
             (hunchentoot:abort-request-handler))))
    (multiple-value-bind (*route* bindings)
        (routes:match (acceptor-routes-mapper (hunchentoot:acceptor-name acceptor))
          (hunchentoot:request-uri request))
      (not-found-if-null *route*)
      (handler-bind ((error #'hunchentoot:maybe-invoke-debugger))
        (let ((result (process-route acceptor *route* bindings)))
          (cond
            ((pathnamep result)
             (hunchentoot:handle-static-file
              result
              (or (hunchentoot:mime-type result)
                  (hunchentoot:content-type hunchentoot:*reply*))))
            (t result)))))))

(defclass easy-routes-acceptor (hunchentoot:easy-acceptor)
  ()
  (:documentation "This acceptor tries to match and handle easy-routes first, but fallbacks to easy-routes dispatcher if there's no matching"))

(defclass easy-routes-ssl-acceptor (easy-routes-acceptor hunchentoot:ssl-acceptor)
  ()
  (:documentation "As for EASY-ROUTES-ACCEPTOR, but works with the hunchentoot EASY-SSL-ACCEPTOR instead."))

(defmethod hunchentoot:acceptor-dispatch-request ((acceptor easy-routes-acceptor) request)
  (multiple-value-bind (*route* bindings)
      (routes:match (acceptor-routes-mapper (hunchentoot:acceptor-name acceptor))
        (hunchentoot:request-uri request))
    (if (not *route*)
        ;; Fallback to dispatch via easy-handlers
        (call-next-method)
        ;; else, a route was matched
        (handler-bind ((error #'hunchentoot:maybe-invoke-debugger))
          (let ((result (process-route acceptor *route* bindings)))
            (cond
              ((pathnamep result)
               (hunchentoot:handle-static-file
                result
                (or (hunchentoot:mime-type result)
                    (hunchentoot:content-type hunchentoot:*reply*))))
              (t result)))))))

(defclass route (routes:route)
  ((symbol :initarg :symbol
           :reader route-symbol)
   (variables :initarg :variables
              :reader variables)
   (required-method :initarg :required-method
                    :initform nil
                    :reader required-method)
   (decorators :initarg :decorators
               :initform nil
               :reader route-decorators
               :type list)))

(defmethod print-object ((route route) stream)
  (print-unreadable-object (route stream :type t :identity t)
    (with-slots (symbol required-method routes::template) route
      (format stream "~A: ~A ~S" symbol required-method routes::template))))

(defmethod routes:route-check-conditions ((route route) bindings)
  (with-slots (required-method) route
    (and required-method
         (eql (hunchentoot:request-method*) required-method)
         t)))

(defmethod routes:route-name ((route route))
  (string-downcase (write-to-string (slot-value route 'symbol))))

(defun call-decorator (decorator next)
  (if (listp decorator)
      (apply (first decorator) (append (rest decorator) (list next)))
      (funcall decorator next)))

(defun call-with-decorators (decorators function)
  (if (null decorators)
      (funcall function)
      (call-decorator (first decorators)
                      (lambda ()
                        (call-with-decorators (rest decorators) function)))))

(defgeneric process-route (acceptor route bindings))

(defmethod process-route ((acceptor hunchentoot:acceptor) (route route) bindings)
  (call-with-decorators
   (route-decorators route)
   (lambda ()
     (apply (route-symbol route)
            (loop for item in (slot-value route 'variables)
                  collect (cdr (assoc item bindings
                                      :test #'string=)))))))

(defun connect-routes (acceptor-name)
  (let* ((routes-and-mapper (if acceptor-name
                                (ensure-acceptor-routes-and-mapper acceptor-name)
                                (list :routes *routes* :routes-mapper *routes-mapper*)))
         (routes (getf routes-and-mapper :routes))
         (routes-mapper (getf routes-and-mapper :routes-mapper)))
    (routes:reset-mapper routes-mapper)
    (loop for route being the hash-values of routes
          do
             (routes:connect routes-mapper route))))

(defmethod make-load-form ((var routes:variable-template) &optional env)
  (declare (ignorable env))
  `(routes::make-variable-template ',(routes::template-data var)))

(defmacro defroute (name template-and-options params &body body)
  "Macro for defining a route.

Syntax:

(defroute <name> (<path> &rest <route-options>) <route-params>
   &body body)

with:

* path: A string with an url path that can contain arguments prefixed with a colon.
  Like \"/foo/:x/:y\", where :x and :y are bound into x and y variables in the context of the route body.
* route-options: possible options are
     * :method - The HTTP method to dispatch, as a keyword. Default is :get.
     * :decorators - The decorators to attach.
     * :acceptor-name - The name of the acceptor the route should be added to (optional).
* route-params: a list of params to be extracted from the url or HTTP request body (POST).
     Has this form: (params &get get-params &post post-params &path path-params), with the &get, &post and &path params sections being optional, and where params are grabbed via HUNCHENTOOT:PARAMETER function, get-params via HUNCHENTOOT:GET-PARAMETER function, and post-params via HUNCHENTOOT:POST-PARAMETER function. path-params specifies the type of params in the url path.

    For example:

    (easy-routes:defroute name (\"/foo/:x\") (y &get z)
        (format nil \"x: ~a y: ~a z: ~a\ x y z))

    Also, params can have Hunchentoot easy-handler style options, described here: http://weitz.de/hunchentoot/#define-easy-handler

    (var &key real-name parameter-type init-form request-type)

    For example:

    (easy-routes:defroute foo \"/foo/:x\"
        ((y :real-name \"Y\" :init-form 22 :parameter-type 'integer))
      (format nil \"~A - ~A\" x y))

    You can also specify the type of path parameters after &path. For example, say you want to sum a path argument to a query argument. You can specify their type as 'INTEGER and calculate their sum without parsing:

    (easy-routes:defroute foo \"/foo/:x\"
        ((y :init-form 10 :parameter-type 'integer)
            &path (x 'integer))
                  (format nil \"~A\" (+ x y)))"

  (let* ((template (if (listp template-and-options)
                       (first template-and-options)
                       template-and-options))
         (variables (routes:template-variables
                     (routes:parse-template template)))
         (arglist (mapcar (alexandria:compose #'intern #'symbol-name)
                          variables))
         (method (or (and (listp template-and-options)
                          (getf (rest template-and-options) :method))
                     :get))
         (acceptor-name (and (listp template-and-options)
                             (getf (rest template-and-options) :acceptor-name)))
         (decorators (and (listp template-and-options)
                          (getf (rest template-and-options) :decorators)))
         (declarations (loop
                         for x = (first body)
                         while (and (listp x) (equalp (first x) 'declare))
                         do (pop body)
                         collect x)))
    (assoc-bind ((params nil)
                 (get-params :&get)
                 (post-params :&post)
                 (path-params :&path))
        (lambda-list-split '(:&get :&post :&path) params)
      `(let ((%route (make-instance 'route
                                    :symbol ',name
                                    :template ',(routes:parse-template template)
                                    :variables ',variables
                                    :required-method ',method
                                    :decorators ',decorators)))
         ,(if acceptor-name
              `(let ((%routes-and-mapper (ensure-acceptor-routes-and-mapper ',acceptor-name)))
                 (setf (gethash ',name (getf %routes-and-mapper :routes)) %route))
              `(setf (gethash ',name *routes*) %route))
         (connect-routes ',acceptor-name)
         (defun ,name ,arglist
           ,@declarations
           (let (,@(loop for param in params
                         collect
                         (hunchentoot::make-defun-parameter param ''string :both))
                 ,@(loop for param in get-params
                         collect
                         (hunchentoot::make-defun-parameter param ''string :get))
                 ,@(loop for param in post-params
                         collect
                         (hunchentoot::make-defun-parameter param ''string :post))
                 ,@(loop for param in path-params
                         collect
                         (destructuring-bind (parameter-name parameter-type) param
                           `(,parameter-name (hunchentoot::convert-parameter ,parameter-name ,parameter-type)))))
             ,@body))))))

(defun find-route (name &key acceptor-name)
  "Find a route by name (symbol)"
  (let ((routes (if acceptor-name (acceptor-routes acceptor-name)
                    *routes*)))
    (gethash name routes)))

;; Code here is copied almost exactly from restas library by Moskvitin Andrey

;; Url generation from route name

(defun route-symbol-template (route-symbol)
  (routes:parse-template (find-route route-symbol)))

(defmethod make-route-url ((tmpl list) args)
  (let* ((uri (make-instance 'puri:uri))
         (bindings (loop for rest on args by #'cddr
                         for key = (first rest)
                         for value = (second rest)
                         collect
                         (cons key
                               (if (or (stringp value) (consp value))
                                   value
                                   (write-to-string value)))))
         (query-part (set-difference bindings
                                     (routes:template-variables tmpl)
                                     :test (alexandria:named-lambda
                                               known-variable-p (pair var)
                                             (eql (car pair) var)))))
    (setf (puri:uri-parsed-path uri)
          (cons :absolute
                (routes::apply-bindings tmpl bindings)))
    (when query-part
      (setf (puri:uri-query uri)
            (format nil
                    "~{~(~A~)=~A~^&~}"
                    (alexandria:flatten query-part))))
    uri))

(defmethod make-route-url ((route symbol) args)
  (make-route-url (or (find-route route :acceptor-name (getf args :acceptor-name))
                      (error "Unknown route: ~A" route))
		  (alexandria:remove-from-plist args :acceptor-name)))

(defmethod make-route-url ((route route) args)
  (make-route-url (routes:route-template route) args))

(defun genurl (route-symbol &rest args &key &allow-other-keys)
  "Generate a relative url from a route name and arguments"
  (puri:render-uri (make-route-url route-symbol args) nil))

(defun parse-host-and-port (host-header)
  "Parse host and port from a Host HTTP reader."
  (let ((parsed (split-sequence:split-sequence #\: host-header)))
    (values (first parsed)
	    (when (second parsed)
	      (parse-integer (second parsed))))))

(defun genurl* (route-symbol &rest args &key &allow-other-keys)
  "Generate an absolute url from a route name and arguments.

Looks at HUNCHENTOOT:*REQUEST* and HUNCHENTOOT:*ACCEPTOR* to infer host and uri scheme. If HUNCHENTOOT:*REQUEST* and HUNCHENTOOT:*ACCEPTOR* are not bound, then \"http\" and \"localhost\" are used as uri scheme and host."
  (let ((uri-scheme
	  (if (boundp 'hunchentoot:*acceptor*)
	      (if (hunchentoot:acceptor-ssl-p hunchentoot:*acceptor*)
                  :https
                  :http)
	      (progn
		(warn "Cannot infer uri scheme. Using \"http\". In EASY-ROUTES:GENURL*.")
		:http)))
	(host
	  (cond
	    ((boundp 'hunchentoot:*request*)
	     (parse-host-and-port (hunchentoot:host)))
	    ((boundp 'hunchentoot:*acceptor*)
	     (hunchentoot:acceptor-address hunchentoot:*acceptor*))
	    (t
	     (warn "Cannot infer host. Using \"localhost\". In EASY-ROUTES:GENURL*.")
	     "localhost")))
	(port (cond
		((boundp 'hunchentoot:*request*)
		 (second (multiple-value-list (parse-host-and-port (hunchentoot:host)))))
		((boundp 'hunchentoot:*acceptor*)
		 (hunchentoot:acceptor-port hunchentoot:*acceptor*))))
	(url (make-route-url route-symbol args)))
    (setf (puri:uri-scheme url) uri-scheme
          (puri:uri-host url) host
	  (puri:uri-port url) port)
    (puri:render-uri url nil)))

;; Redirect

(defun apply-format-aux (format args)
  (if (symbolp format)
      (apply #'genurl format args)
      (if args
          (apply #'format nil (cons format args))
          format)))

(defun redirect (route-symbol &rest args)
  "Redirect to a route with name ROUTE-SYMBOL.
ARGS is a property list with route parameters."
  (hunchentoot:redirect
   (hunchentoot:url-decode
    (apply-format-aux route-symbol
                      (mapcar #'(lambda (s)
                                  (if (stringp s)
                                      (hunchentoot:url-encode s)
                                      s))
                              args)))))

;; Copied code ends here

;; Decorators

(defun @html (next)
  "HTML decoration. Sets reply content type to text/html"
  (setf (hunchentoot:content-type*) "text/html")
  (funcall next))

(defun @json (next)
  "JSON decoration. Sets reply content type to application/json"
  (setf (hunchentoot:content-type*) "application/json")
  (funcall next))

(defun @check (predicate http-error next)
  "Decorator that checks if PREDICATE evaluation is true.
PREDICATE is a funcallable object.
If the check succeeds, then the NEXT middleware is called.
If the check fails, then the request is aborted with HTTP status HTTP-ERROR.

Example usage:

(defroute my-route (\"/my-route\" :method :get
                                :decorators ((@check my-permissions-checking-function hunchentoot:+http-forbidden+)))
  ...
)"
  (if (funcall predicate)
      (funcall next)
      (http-error http-error)))

(defun @check-permission (predicate next)
  "Decorator that aborts the current request with a HTTP permission denied error if the evaluation of PREDICATE is false.
PREDICATE is a funcallable object."
  (if (funcall predicate)
      (funcall next)
      (permission-denied-error)))

;; HTTP Errors

(defun http-error (http-error)
  "Abort current handler and signal HTTP error HTTP-ERROR.

HTTP-ERROR should be an HTTP status code (integer)."
  (setf (hunchentoot:return-code*) http-error)
  (hunchentoot:abort-request-handler))

(defun not-found-error ()
  "Aborts current handler and returns with an HTTP not found error."
  (http-error hunchentoot:+http-not-found+))

(defun permission-denied-error ()
  "Aborts current handler and returns with an HTTP forbidden error."
  (http-error hunchentoot:+http-forbidden+))

(defun or-http-error (value http-error)
  "Utility function for signaling HTTP-ERROR if VALUE is null.

HTTP-ERROR should be an HTTP status code (integer)."
  (or value
      (http-error http-error)))

(defun or-not-found (value)
  "Utility function for signaling an HUNCHENTOOT:+HTTP-NOT-FOUND+ error if VALUE is null.

Use in your routes like:

(let ((my-object (or-not-found (find-my-object id))))
   ...)

where id is a route parameter.

The route retuns an HTTP not found error if object with that id could not be found.
"
  (or-http-error value hunchentoot:+http-not-found+))
