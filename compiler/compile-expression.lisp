
					;TODO: Check function is defined
(defun compile-expr (expr env functions-definitions instructions-list)
  (cond
    ((equal (car expr) 'defun)
     (setq functions-definitions (append functions-definitions (compile-defun (cadr expr) (caddr expr) (cdddr expr)))))
    ((is-arithmetic-expression expr) (compile-arithmetic-expression (car expr) (cdr expr) env))
    (t (compile-function-call (car expr) (cdr expr) instructions-list)))
  t)

(compile-expr '(defun add (x y) (+ x y)) nil nil nil)
