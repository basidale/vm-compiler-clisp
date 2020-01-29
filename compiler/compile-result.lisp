					;TODO: Rename compiler-result
(defmacro result-code-cell (result)
  `(assoc 'code ,result))

(defun result-code (result)
  (cdr (result-code-cell result)))

(defun result-is-function-definition (result)
  (cdr (assoc 'function-definition result)))

(defun make-result (code &key function-definition)
  (list (cons 'code code)
	(cons 'function-definition function-definition)))
