					;TODO: Replace loop by map

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





