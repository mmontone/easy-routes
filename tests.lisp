(require :hunchentoot)
(require :easy-routes)
(require :drakma)

(defpackage :easy-routes-tests
  (:use :cl :easy-routes)
  (:export #:run-tests))

(in-package :easy-routes-tests)

(easy-routes:defroute integer-param-test-1
    ("/tests/integer-param-1/:x"
     :method :get
     :acceptor-name easy-routes-tests)
    (&path (x 'integer))
  (assert (typep x 'integer))
  (prin1-to-string x))

(easy-routes:defroute integer-param-test-2
    ("/tests/integer-param-2/:x"
     :method :get
     :acceptor-name easy-routes-tests)
    (&path (x 'integer) &get (y :real-name "Y" :init-form 22 :parameter-type 'integer))
  (assert (typep x 'integer))
  (assert (typep y '(or null integer)))
  (concatenate 'string (prin1-to-string x) (prin1-to-string y)))

(easy-routes:defroute array-param-test
    ("/tests/array-param"
     :method :get
     :acceptor-name easy-routes-tests)
    (&get (x :parameter-type '(array string)))
  (princ-to-string x))

(easy-routes:defroute list-param-test
    ("/tests/list-param"
     :method :get
     :acceptor-name easy-routes-tests)
    (&get (x :parameter-type '(list string)))
  (princ-to-string x))

(easy-routes:defroute hash-param-test
    ("/tests/hash-param"
     :method :get
     :acceptor-name easy-routes-tests)
    (&get (x :parameter-type '(hash-table string)))
  (princ-to-string x))

(defvar *test-service* nil)

(defun start-test-service ()
  (setf *test-service*
        (hunchentoot:start (make-instance 'easy-routes-acceptor :name 'easy-routes-tests :port 0))))

(defun stop-test-service ()
  (hunchentoot:stop *test-service*)
  (setf *test-service* nil))

(defun test-request (test-path &rest args)
  (apply #'drakma:http-request
         (format nil "http://localhost:~a/tests/~a"
                 (hunchentoot:acceptor-port *test-service*)
                 test-path)
         args))

(defun run-tests ()
  (start-test-service)
  (assert (string= (test-request "integer-param-1/33")
                   "33"))
  (multiple-value-bind (reply status)
      (test-request "integer-param-1/lala")
    (assert (and (string= reply "lala should be a INTEGER")
                 (= status 400))))
  (assert (string= (test-request "integer-param-2/33")
                   "3322"))
  (multiple-value-bind (reply status)
      (test-request "integer-param-2/foo")
    (assert (and (string= reply "foo should be a INTEGER")
                 (= status 400))))
  ;; FIXME: Y value should signal error
  (multiple-value-bind (reply status)
      (test-request "integer-param-2/44?Y=lala")
    (assert (and (string= reply "Y should be a INTEGER")
                 (= status 400))))
  #+fixme
  (let ((puri:*strict-parse* nil))
    (test-request "array-param?x[0]=foo&x[1]=bar&x[2]=baz"))
  (assert (string= (test-request "list-param?x=foo&x=bar&x=baz")
                   "(foo bar baz)"))
  #+fixme(let ((puri:*strict-parse* nil))
           (test-request "hash-param?x{foo}=bar"))
  (stop-test-service)
  t)

(run-tests)
