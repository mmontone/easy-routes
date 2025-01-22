(in-package :easy-routes)

;; Hunchentoot's parameter computation does not signal type errors.
;; This is a rewrite that signals HTTP BAD REQUEST errors when a parameter
;; with a specified type could not be parsed.

(defun bad-request (message &rest args)
  (setf (tbnl:return-code tbnl:*reply*) tbnl:+http-bad-request+)
  (tbnl:abort-request-handler (apply #'format nil message args)))

;; similar to Hunchentoot's, but signals error
(defun convert-parameter (argument type)
  "Converts the string ARGUMENT to TYPE where TYPE is one of the
symbols STRING, CHARACTERS, INTEGER, KEYWORD, or BOOLEAN - or
otherwise a function designator for a function of one argument.
ARGUMENT can also be NIL in which case this function also returns
NIL unconditionally."
  (when (listp argument)
    ;; this if for the case that ARGUMENT is NIL or the result of a
    ;; file upload
    (return-from convert-parameter argument))
  (case type
    (string argument)
    (character (or (and (= (length argument) 1)
                        (char argument 0))
                   (bad-request "Not a character: ~s" argument)))
    (integer (handler-case (parse-integer argument)
               (error ()
                 (bad-request "Not an integer: ~s" argument))))
    (keyword (tbnl::as-keyword argument :destructivep nil))
    (boolean t)
    (otherwise (funcall type argument))))

(defun compute-simple-parameter (parameter-name type parameter-reader)
  "Retrieves the parameter named PARAMETER-NAME using the reader
PARAMETER-READER and converts it to TYPE."
  (convert-parameter (funcall parameter-reader parameter-name) type))

(defun compute-list-parameter (parameter-name type parameters)
  "Retrieves all parameters from PARAMETERS which are named
PARAMETER-NAME, converts them to TYPE, and returns a list of
them."
  (loop for (name . value) in parameters
        when (string= name parameter-name)
        collect (convert-parameter value type)))

(defun compute-array-parameter (parameter-name type parameters)
  "Retrieves all parameters from PARAMETERS which are named like
\"PARAMETER-NAME[N]\" \(where N is a non-negative integer),
converts them to TYPE, and returns an array where the Nth element
is the corresponding value."
  ;; see <http://common-lisp.net/pipermail/tbnl-devel/2006-September/000660.html>
  #+:sbcl (declare (sb-ext:muffle-conditions warning))
  (let* ((index-value-list
          (loop for (full-name . value) in parameters
                for index = (ppcre:register-groups-bind (name index-string)
                                ("^(.*)\\[(\\d+)\\]$" full-name)
                              (when (string= name parameter-name)
                                (parse-integer index-string)))
                when index
                collect (cons index (convert-parameter value type))))
         (array (make-array (1+ (reduce #'max index-value-list
                                        :key #'car
                                        :initial-value -1))
                            :initial-element nil)))
    (loop for (index . value) in index-value-list
          do (setf (aref array index) value))
    array))

(defun compute-hash-table-parameter (parameter-name type parameters key-type test-function)
  "Retrieves all parameters from PARAMETERS which are named like
\"PARAMETER-NAME{FOO}\" \(where FOO is any sequence of characters
not containing curly brackets), converts them to TYPE, and
returns a hash table with test function TEST-FUNCTION where the
corresponding value is associated with the key FOO \(converted to
KEY-TYPE)."
  (let ((hash-table (make-hash-table :test test-function)))
    (loop for (full-name . value) in parameters
          for key = (ppcre:register-groups-bind (name key-string)
                        ("^(.*){([^{}]+)}$" full-name)
                      (when (string= name parameter-name)
                        (convert-parameter key-string key-type)))
          when key
          do (setf (gethash key hash-table)
                   (convert-parameter value type)))
    hash-table))

(defun compute-parameter (parameter-name parameter-type request-type)
  "Computes and returns the parameter\(s) called PARAMETER-NAME
and converts it/them according to the value of PARAMETER-TYPE.
REQUEST-TYPE is one of :GET, :POST, or :BOTH."
  (when (member parameter-type '(list array hash-table))
    (setq parameter-type (list parameter-type 'string)))
  (let ((parameter-reader (ecase request-type
                              (:get #'tbnl:get-parameter)
                              (:post #'tbnl:post-parameter)
                              (:both #'tbnl:parameter)))
        (parameters (and (listp parameter-type)
                         (case request-type
                           (:get (tbnl:get-parameters*))
                           (:post (tbnl:post-parameters*))
                           (:both (append (tbnl:get-parameters*) (tbnl:post-parameters*)))))))
    (cond ((atom parameter-type)
           (compute-simple-parameter parameter-name parameter-type parameter-reader))
          ((and (null (cddr parameter-type))
                (eq (first parameter-type) 'list))
           (compute-list-parameter parameter-name (second parameter-type) parameters))
          ((and (null (cddr parameter-type))
                (eq (first parameter-type) 'array))
           (compute-array-parameter parameter-name (second parameter-type) parameters))
          ((and (null (cddddr parameter-type))
                (eq (first parameter-type) 'hash-table))
           (compute-hash-table-parameter parameter-name (second parameter-type) parameters
                                         (or (third parameter-type) 'string)
                                         (or (fourth parameter-type) 'equal)))
          (t (tbnl::parameter-error "Don't know what to do with parameter type ~S." parameter-type)))))

(defun make-defun-parameter (description default-parameter-type default-request-type)
  "Creates a keyword parameter to be used by DEFINE-EASY-HANDLER.
DESCRIPTION is one of the elements of DEFINE-EASY-HANDLER's
LAMBDA-LIST and DEFAULT-PARAMETER-TYPE and DEFAULT-REQUEST-TYPE
are the global default values."
  (when (atom description)
    (setq description (list description)))
  (destructuring-bind (parameter-name &key (real-name (tbnl::compute-real-name parameter-name))
                                           parameter-type init-form request-type)
      description
    `(,parameter-name (or (and (boundp '*request*)
                               (compute-parameter ,real-name
                                                  ,(or parameter-type default-parameter-type)
                                                  ,(or request-type default-request-type)))
                          ,init-form))))
