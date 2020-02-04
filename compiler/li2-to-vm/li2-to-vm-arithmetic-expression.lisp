					;TODO: One function for 4 operations

(defun compile-arithmetic-operation-set-registers (args env)
  `((move ,(car args) R1)
    (move ,(cadr args) R0)))

(defun compile-add(args env)
  (append (compile-arithmetic-operation-set-registers args env)
	  `((add R1 R0))))

(defun compile-sub(args env)
  (append (compile-arithmetic-operation-set-registers args env)
	  `((sub R1 R0))))

(defun compile-mul(args env)
  (append (compile-arithmetic-operation-set-registers args env)
	  `((mul R1 R0))))

(defun compile-div(args env)
  (append (compile-arithmetic-operation-set-registers args env)
	  `((div R1 R0))))

(defun arithmetic-compiler (operator)
  (cdr (assoc operator (arithmetic-operators))))

(defun is-arithmetic-expression (expr)
  (not (null (assoc (car expr) (arithmetic-operators)))))

(defun compile-arithmetic-expression (operator args env)
  (funcall (arithmetic-compiler operator) args env))

(defun arithmetic-operators ()
  '((:add . compile-add)
    (:sub . compile-sub)
    (:mul . compile-mul)
    (:div . compile-div)))
