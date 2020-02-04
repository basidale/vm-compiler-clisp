(defun fp-mapping (env)
  (let ((index (length env)))
    (map 'list
	 (lambda (arg)
	   (let ((pair `(,arg . (fp ,(- index)))))
	     (setq index (- index 1))
	     pair))
	 env)))

(defun fp-get (identifier env)
  (let ((cell (assoc identifier (fp-mapping env))))
    (if (null cell)
	(error "Variable ~S is not bound" identifier)
	(cdr cell))))

(defun map-compile-argument (expr env)
  (if (null expr)
      nil
      (cons(compile-argument (car expr) env)
	   (map-compile-argument (cdr expr) env))))

(defun compile-argument (expr env)
  (cond
    ((equal (car expr) :CONST)
     expr)
    ((equal (car expr) :ARG)
     (fp-get (cadr expr) env))
    (t (error "~S is not a valid argument" expr))))

