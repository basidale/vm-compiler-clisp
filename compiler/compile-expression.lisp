
					;TODO: Check function is defined
(defun compile-expr (expr env)
  (let ((result nil))
    (cond
      ((not (consp expr))
       (error "Symbol compilation not implemented"))
      ((equal (car expr) 'defun)
       (setq result (compile-defun (cadr expr) (caddr expr) (cdddr expr))))
      ((is-arithmetic-expression expr)
       (setq result (compile-arithmetic-expression (car expr) (cdr expr) env)))
      (t
       (setq result (compile-function-call (car expr) (cdr expr)))))
    result))
