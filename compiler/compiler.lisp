(require "compile-expression.lisp")
(require "compile-arithmetic-expression.lisp")
(require "compile-function-definition.lisp")
(require "compile-function-call.lisp")

(defun compile-code (code)
  (let ((functions-definitions nil)
	(instructions-list nil))
    (loop for stmt in code do
	 (compile-expr stmt functions-definitions instructions-list))
    (append instructions-list functions-definitions)))

(compile-code '((defun add (x y) (+ x y)) (add 1 2)))


