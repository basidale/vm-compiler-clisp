
					;TODO: Check function is defined
(defun compile-expr (expr env)
  (let ((result nil))
    (cond
      ((is-arithmetic-expression expr)
       (setq result (compile-arithmetic-expression (car expr) (cdr expr) env)))
      (t
       (setq result (compile-function-call (car expr) (cdr expr) instructions-list))))
    result))
  

(compile-expr '(defun add (x y) (+ x y)) nil nil nil)
