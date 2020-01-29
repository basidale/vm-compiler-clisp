(require 'compile-result "./compiler/compile-result.lisp")
(require 'compile-arithmetic-expression "./compiler/compile-arithmetic-expression.lisp")
(require 'compile-function-call "./compiler/compile-function-call.lisp")
(require 'compile-expression "./compiler/compile-expression.lisp")
(require 'compile-function-definition "./compiler/compile-function-definition.lisp")

					;TODO: Replace loop by car
(defun compile-on-halt ()
  '((halt)))

(defun compile-code (code)
  (let ((functions nil)
	(instructions nil))
    (loop for expr in code do
      (let ((result (compile-expr expr nil)))
	(if (result-is-function-definition result)
	    (setq functions (append functions (result-code result)))
	    (setq instructions (append instructions (result-code result))))))
    (append instructions (compile-on-halt) functions)))





