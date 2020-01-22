					;TODO: use &rest and apply instead of funcall

(defun arithmetic-operators ()
  '((+ . compile-add)
    (- . compile-sub)
    (* . compile-mul)
    (/ . compile-div)))

(defun compile-arithmetic-operation-set-registers (args env)
  (let ((arg1 (args-get-mapping (car args) env))
	(arg2 (args-get-mapping (cadr args) env)))
    (if (null arg1)
	(error "Variable ~S is not bound" (car args)))
    (if (null arg2)
	(error "Variable ~S is not bound" (cadr args)))
    `((move ,arg1 R1)
      (move ,arg2 R0))))

(defun compile-add(args env)
  (append (compile-arithmetic-operation-set-registers args env)
	  '((add R1 R0))))

(defun compile-sub(args env)
  (error "compile-sub not implemented"))

(defun compile-mul(args env)
  (error "compile-mul not implemented"))

(defun compile-div(args env)
  (error "compile-div not implemented"))

(defun arithmetic-compiler (operator)
  (cdr (assoc operator (arithmetic-operators))))

(defun is-arithmetic-expression (expr)
  (not (null (assoc (car expr) (arithmetic-operators)))))

(defun compile-arithmetic-expression (operator args env)
  (funcall (arithmetic-compiler operator) args env))

