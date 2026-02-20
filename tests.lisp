(require :hunchentoot)
(require :easy-routes)
(require :drakma)

(defpackage :easy-routes-tests
  (:use :cl :easy-routes))

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
  (assert (typep y 'integer))
  (concatenate 'string (prin1-to-string x) (prin1-to-string y)))

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
  (assert (test-request "integer-param-1/lala")) ;;; what should this be??
  (assert (string= (test-request "integer-param-2/33")
                   "3322"))
  (stop-test-service)
  t)

;; (run-tests)
