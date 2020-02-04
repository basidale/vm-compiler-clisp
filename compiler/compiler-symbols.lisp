(defmacro compiler-symbols-cell (compiler)
  `(assoc 'symbols ,compiler))

(defun compiler-symbols (compiler)
  (cdr (compiler-symbols-cell compiler)))

(defun compiler-add-to-symbols (symbol value compiler)
  (setf (cdr (compiler-symbols-cell)) (acons symbol value (compiler-symbols compiler))))

(defun compiler-symbol-value (symbol compiler)
  (let ((symbol (cdr (assoc symbol (compiler-symbols compiler)))))
    (if (null symbol)
	(compiler-add-to-symbols symbol (length (compiler-symbols compiler)) compiler)
	symbol)))
