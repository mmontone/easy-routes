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

### Parameters: ###

Parameters in the url path are prefixed with a colon, and they are bound into the route body, so they are accessible from there. Also, there are options to grab parameters from the url query section after the question mark, and also post parameters.

    * `:get-params <list of params>` - Grabs parameters from the url using `hunchentoot:get-parameter` function, and bounds them into the route body.
    * `:post-params <list of params>` - Grabs parameters from the HTTP post body using `hunchentoot:post-parameter` function, and bounds them into the route body.
    * `:params <list of params>` - Grabs either the "GET" or the "POST" params via `hunchentoot:post-parameter` function, and bounds them into the route body.
    
#### Example: ####

```lisp
(easy-routes:defroute name ("/foo/:x" :params (y) :get-params (z))
           (format nil "x: ~a y: ~y z: ~a" x y z))
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
