(in-package #:easy-routes)

(defparameter *indention* 4)

(defparameter *current-indention* nil)

(defgeneric print-route (route stream))

(defmethod print-route (route stream)
  (format stream "~a" route))

(defmethod print-route ((route route) stream)
  (princ (route-symbol route) stream))

(defmethod print-route ((route routes::variable-template) stream)
  (format stream "{~(~A~)}" (routes::template-data route)))

(defmethod print-route ((route routes::wildcard-template) stream)
  (format stream "*~(~A~)" (routes::template-data route)))

(defmethod print-route ((route cons) stream)
  (cond
    ((typep (car route) 'routes::route)
     (princ (required-method (car route)) stream)
     (write-char #\space stream)
     (write-string (routes::route-name (car route)) stream))
    ((and (typep (cdr route) 'cons)
          (typep (car (cdr route)) 'route))
     (print-route (car route) stream)
     (if (equal (car route) "")
         nil
         (write-char #\space stream))
     (print-route (cdr route) stream))
    (t (print-route (car route) stream)
       (write-string "/" stream)
       (print-route (cdr route) stream))))

(defmethod print-route ((route routes::or-template) stream)
  (let ((*current-indention* (if (null *current-indention*)
                                 0
                                 (+ *current-indention* *indention*))))
    (loop for item in (routes::template-data route)
       do
         (write-char #\newline stream)
         (write-string (make-string *current-indention* :initial-element #\Space) stream)
         (print-route item stream))))

(defmethod print-route ((route routes::concat-template) stream)
  (loop for item in (routes::template-data route)
     do (print-route item stream)))

(defmethod describe-object ((mapper routes:mapper) stream)
  (call-next-method)
  (format stream "~%Tree of routes~%--------------~%")
  (print-route (slot-value mapper 'routes::template) stream))
