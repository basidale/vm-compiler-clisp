(require 'compile-arithmetic-expression "./compiler/compile-arithmetic-expression.lisp")
(require 'compile-function-call "./compiler/compile-function-call.lisp")
(require 'compile-expression "./compiler/compile-expression.lisp")
(require 'compile-function-definition "./compiler/compile-function-definition.lisp")
(require 'compile-results "./compiler/compile-results.lisp")

;; (require 'codecompute "../res/compute/compute.lisp")

(defun compile-on-halt ()
  '((halt)))

(defun compile-code (code)
  (let ((functions-definitions nil)
	(instructions-list nil)
	(symbol-table nil))
    (loop for expr in code do
      (let ((results nil))
	(if (equal (car expr) 'defun)
	    (setq results (compile-defun (cadr expr) (caddr expr) (cdddr expr)))
	    (setq results (compile-expr expr nil)))
	(let ((functions-definitions (compile-get-function-definition))
	      (instructions-list (compiler-get-instructions-list)))
	  (append instructions-list (compile-on-halt) functions-definitions))))

(compile-code basecode)



