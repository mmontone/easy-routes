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

### Syntax: ###

```lisp

(defroute <name> (<path> &rest <route-options>) <route-params> 
   &body body)
   
```

with:

* `path`: A string with an url path that can contain arguments prefixed with a colon. 
  Like `"/foo/:x/:y"`, where `:x` and `:y` are bound into x and y variables in the context of the route body.
* `route-options`: possible options are
     * `:method` - The HTTP method to dispatch. Either `:get` or `:post`.
     * `:decorators` - The decorators to attach (see below).
* `route-params`: a list of params to be extracted from the url or HTTP request body (POST). 
     Has this form: `(params &get get-params &post post-params)` where `params` are grabbed via `hunchentoot:parameter` function, `get-params` via `hunchentoot:get-parameter` function, and `post-params` via `hunchentoot:post-parameter` function.
        
    For example:

    ```lisp
    (easy-routes:defroute name ("/foo/:x") (y &get z)
        (format nil "x: ~a y: ~y z: ~a" x y z))
    ```
    Also, params can have Hunchentoot easy-handler style options, described here: http://weitz.de/hunchentoot/#define-easy-handler
    
    ```
    (var &key real-name parameter-type init-form request-type)
    ```
       
    For example:
    
    ```lisp
    (easy-routes:defroute foo "/foo/:x" 
        ((y :real-name "Y" :init-form 22 :parameter-type 'integer))
      (format nil "~A - ~A" x y)) 
    ```

Example route:

```lisp
(defroute foo ("/foo/:arg1/:arg2" :method :get
                                  :decorators (@auth @db @html))
   (&get w)
    (format nil "<h1>FOO arg1: ~a arg2: ~a ~a</h1>" arg1 arg2 w))
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
