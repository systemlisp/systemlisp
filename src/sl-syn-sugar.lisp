(in-package :system-lisp)

(defun nest-expr (lst)
  (cond
    ((null (cdr lst)) lst)     
    ((null (cddr lst)) lst)    
    (t (list (car lst) (nest-expr (cdr lst))))))

(defun field-expr (expr)
  (nest-expr (reverse expr)))

(defmacro -> (&rest args)
  (field-expr args))

(defun method-expr (path args)
  `(,@(field-expr path) ,@args))

(defmacro f-> (method-path &rest args)
  (method-expr method-path args))
