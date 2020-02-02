(defun args-mapping (env)
  (let ((index (length env)))
    (map 'list
	 (lambda (arg)
	   (let ((pair `(,arg . (fp ,(- index)))))
	     (setq index (- index 1))
	     pair))
	 env)))

(defun args-get-mapping (identifier env)
  (let ((cell (assoc identifier (args-mapping env))))
    (if (null cell)
	(error "Variable ~S is not bound" identifier)
	(cdr cell))))

(defun compile-argument (expr env)
  (args-get-mapping (cadr expr) env))
