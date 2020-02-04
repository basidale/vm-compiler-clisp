					;TODO: One function for 4 operations
(defun arithmetic-operators ()
  '((:add . add)
    (:sub . sub)
    (:mul . mul)
    (:div . div)))

(defun arithmetic-compiler (operator)
  (cdr (assoc operator (arithmetic-operators))))

(defun is-arithmetic-expression (expr)
  (not (null (assoc (car expr) (arithmetic-operators)))))

(defun compile-arithmetic-expression (operator args env compiler)
  (append (li2-to-vm-compile-expr (car args) env compiler)
	  (list (list 'move 'R0 'R1))
	  (li2-to-vm-compile-expr (cadr args) env compiler)
	  (list (list (arithmetic-compiler operator) 'R1 'R0))))
