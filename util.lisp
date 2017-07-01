(in-package :easy-routes)

;; From CEPL and Varjo source code by Chris Bagley (Baggers) <techsnuffle@gmail.com>

(defun lambda-list-split (template lam-list)
  (labels ((kwd (x) (intern (format nil "~a" x) :keyword))
           (symbol-name= (x y) (equal (symbol-name x) (symbol-name y)))
           (collector (lam-list &optional current-modifier accum)
             (let ((item (first lam-list)))
               (cond ((null lam-list) accum)
                     ((and (symbolp item) (eql (elt (symbol-name item) 0) #\&))
                      (collector (rest lam-list)
                                 (kwd item)
                                 accum))
                     (t (collector (rest lam-list)
                                   current-modifier
                                   (acons current-modifier
                                          (cons item
                                                (cdr (assoc current-modifier
                                                            accum)))
                                          accum))))))
           (clean-alist (alist &optional accum)
             (let ((item (first alist)))
               (cond ((null alist) accum)
                     ((atom item) (clean-alist (rest alist) accum))
                     ((not (assoc (first item) accum))
                      (clean-alist (rest alist) (cons item accum)))
                     (t (clean-alist (rest alist) accum)))))
           (reverse-results (r)
             (loop for (n . rst) in r collect (cons n (reverse rst))))
           (first-in-template-p (x) (or (null (first x))
                                        (member (first x) template
                                                :test #'symbol-name= ))))
    (let ((template (when template (cons nil (mapcar #'kwd template))))
          (split (collector lam-list)))
      (if (or (null template)
              (every #'first-in-template-p split))
          (reverse-results (clean-alist split))
          (let* ((&-syms (remove-if-not
                          (lambda (x)
                            (when (symbolp x) (eq (elt (symbol-name x) 0) #\&)))
                          lam-list))
                 (unknown (remove-if (lambda (x) (member x template))
                                     &-syms)))
            (error "~%Varjo: Found the symbol~a ~a. Given that it starts with '&' it looks
like a lambda list keyword. Unfortunately the only lambda list keywords that
are supported in this context are: ~s"
                   (if (> (length unknown) 1) "s" "")
                   (if (= (length unknown) 1) (first unknown) unknown)
                   (remove nil template)))))))

(defmacro assoc-bind (lambda-list alist &body body)
  (let* ((g (gensym "alist"))
         (bindings (loop :for l :in lambda-list :collect
                      (let ((var (if (listp l) (first l) l))
                            (key (if (listp l)
                                     (second l)
                                     l)))
                        `(,var (cdr (assoc ',key ,g)))))))
    `(let ((,g ,alist))
       (let ,bindings
         (declare (cl:ignorable ,@(mapcar #'first bindings)))
         ,@body))))

;; end
