# EASY-ROUTES #

EASY-ROUTES is yet another routes handling system on top of Hunchentoot.

It's just glue code for Restas routing subsystem (CL-ROUTES).

It supports:

    * Dispatch based on HTTP method
    * Arguments extraction from the url path
    * Decorators

## Usage: ##

Use `routes-acceptor` acceptor:

```lisp
(hunchentoot:start (make-instance 'easy-routes:routes-acceptor))
```

## Routes: ##

```lisp
(defroute foo ("/foo/:arg1/:arg2" :method :get
                                  :decorators (@auth @db @html))
    (format nil "<h1>FOO arg1: ~a arg2: ~a </h1>" arg1 arg2))
```

## Decorators: ##

Decorators are functions that are executed before the route body. They should call the `next` parameter function to continue executing the decoration chain and the route body finally.

### Examples: ###

```lisp
(defun @auth (next)
  (let ((*user* (hunchentoot:session-value 'user)))
    (if (not *user*)
	(hunchentoot:redirect "/login")
	(funcall next))))

(defun @html (next)
  (setf (hunchentoot:content-type*) "text/html")
  (funcall next))

(defun @json (next)
  (setf (hunchentoot:content-type*) "application/json")
  (funcall next))

(defun @db (next)
  (postmodern:with-connection *db-spec*
    (funcall next)))
```
