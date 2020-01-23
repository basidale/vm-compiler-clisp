(require "compile-arithmetic-expression.lisp")
(require "compile-function-call.lisp")
(require "compile-expression.lisp")
(require "compile-function-definition.lisp")

(defun compile-on-halt ()
  '((halt)))

(defun compile-code (code)
  (let ((functions-definitions nil)
	(instructions-list nil))
    (loop for expr in code do
      (if (equal (car expr) 'defun)
	  (setq functions-definitions (append functions-definitions (compile-defun (cadr expr) (caddr expr) (cdddr expr))))
	  (setq instructions-list (append instructions-list (compile-expr expr nil functions-definitions instructions-list)))))
    (append instructions-list (compile-on-halt) functions-definitions)))

(compile-code '((defun add (x y) (+ x y)) (add 1 2)))


