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

Note that the `routes-acceptor` returns with HTTP not found if no route matches and doesn't fallback to `easy-handlers`, and so it doesn't iterate over Hunchentoot `*dispatch-table*`. Most of the time, that iteration is a useful thing, so you may want to start the `easy-routes:easy-routes-acceptor` instead, that inherits from Hunchentoot `easy-acceptor` and so it iterates the dispatch table if no route matches (useful for being able to use `define-easy-handler` and also handling static files).

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
     Has this form: `(params &get get-params &post post-params &path path-params)`, with the `&get`, `&post` and `&path` params sections being optional, and where `params` are grabbed via `hunchentoot:parameter` function, `get-params` via `hunchentoot:get-parameter` function, and `post-params` via `hunchentoot:post-parameter` function. `path-params` specifies the type of params in the url path (see below for an example).
        
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
    
    You can also specify the type of path parameters after `&path`. For example, say you want to sum a path argument to a query argument. You can specify their type as 'INTEGER and calculate their sum without parsing:
    
    ```lisp
    (easy-routes:defroute foo "/foo/:x" 
        ((y :init-form 10 :parameter-type 'integer) 
            &path (x 'integer))
                  (format nil "~A" (+ x y)))
    ```
    
### Example route: ###

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

Decorators also support parameters, like in the `@check` and `@check-permission` decorators:

```lisp
(defun @check (predicate http-error next)
  (if (funcall predicate)
      (funcall next)
      (http-error http-error)))

(defun @check-permission (predicate next)
  (if (funcall predicate)
      (funcall next)
      (permission-denied-error)))
 ```
 
 Then you can use those decorators passing the needed parameters. `predicate` and `http-error` for `@check`, 
 and `predicate` for check permission:
 
 ```lisp
 (defroute my-protected-route ("/foo" :method :get
                                      :decorators (@check my-permissions-checking-function hunchentoot:+http-forbidden+))
	...)
```

## Map of routes visualization

CL-ROUTES package implement special SWANK code for routes map visualization. Just inspect `*ROUTES-MAP*` variable from your lisp listener.

For example:

```
#<ROUTES:MAPPER {1007630E53}>
--------------------

Tree of routes
--------------------------------------------------

users invoice-engine::admin/users
api/invoices/chart invoice-engine::invoices-chart-data
invoice-engine::dashboard
logout invoice-engine::logout
company/logo invoice-engine::company-logo
search invoice-engine::global-search
preview-invoice invoice-engine::preview-invoice
dt-invoices invoice-engine::datatables-list-invoices
tenants invoice-engine::admin/tenants
admin/
    settings invoice-engine::admin/settings
    invoice-engine::admin/dashboard
    tenants/new/
        invoice-engine::admin/tenants/create
        invoice-engine::admin/tenants/new
    login/
        invoice-engine::admin/signin
        invoice-engine::admin/login
    tenant/$id invoice-engine::admin/tenant
    users/
        new/
            invoice-engine::admin/users/create
            invoice-engine::admin/users/new
        $id/edit/
            invoice-engine::admin/users/update
            invoice-engine::admin/users/edit
customers/
    invoice-engine::web/list-customers
    $id invoice-engine::view-customer
invoices/
    invoice-engine::list-invoices-route
    $id/
        print invoice-engine::web/print-invoice
        printed invoice-engine::web/printed-invoice
        invoice-engine::view-invoice
        send invoice-engine::web/send-invoice-by-email

```

Less fancy, but useful too, you can also use `(describe easy-routes:*routes-mapper*)` to visualize the tree of routes.

## Reference ##

## Functions
### @html

```lisp
(next)
```

HTML decoration. Sets reply content type to text/html

### find-route

```lisp
(name)
```

Find a route by name (symbol)

### genurl

```lisp
(route-symbol &rest args &key &allow-other-keys)
```

Generate a relative url from a route name and arguments

### genurl\*

```lisp
(route-symbol &rest args &key &allow-other-keys)
```

Generate an absolute url from a route name and arguments

### redirect

```lisp
(route-symbol &rest args)
```
Redirect to a route url. Pass the route name and the parameters.


## Macros
### defroute

```lisp
(name template-and-options params &body body)
```

Route definition syntax

## Classes

### easy-routes-acceptor
This acceptor tries to match and handle easy-routes first, but fallbacks to easy-routes dispatcher if there's no matching

### routes-acceptor
This acceptors handles routes and only routes. If no route is matched then an HTTP NOT FOUND error is returned.
If you want to use Hunchentoot easy-handlers dispatch as a fallback, use EASY-ROUTES-ACCEPTOR
