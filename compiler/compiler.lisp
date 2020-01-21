
(defun compile-constant ()
  (error "compile-constant not implemented"))

(defun compile-defun-body (args body)
  (print body))

(defun compile-defun (name args body functions-definitions)
  (let ((target `((label ,name)
		  ,(compile-defun-body args body))))))

(defun compile-expr (expr functions-definitions)
(cond
   ((equal (car expr) 'defun) (compile-defun (cadr expr) (caddr expr) (cdddr expr) functions-definitions))
   (t (error "%S is not supported" (car expr)))))

(defun compile (code)
  (let ((functions-definitions nil))
    (loop for stmt in code do
	  (compile-expr stmt functions-definitions))))

(compile '((defun add (x y) (+ x y)) (add 1 2)))


