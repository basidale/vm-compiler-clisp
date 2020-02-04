(defun find-index (identifier env)
  (labels ((recurs (env index)
	     (if (null env)
		 nil
		 (if (equal (car env) identifier)
		     index
		     (recurs (cdr env) (+ index 1))))))
    (recurs env 0)))

(defun fp-get (expr args-env locals-env)
  (cond
    ((equal (car expr) :ARG)
     (let ((index (find-index (cadr expr) args-env)))
       (if (null index)
	   (error "Argument ~S is not bound" (cadr expr))
	   `(fp ,(+ (- (length args-env)) index)))))
    ((equal (car expr) :VAR)
     (let ((index (find-index (cadr expr) locals-env)))
       (if (null index)
	   (error "Local variable ~S is not bound" (cadr expr))
	   `(fp ,(+ 3 index)))))))

(defun compile-argument (expr args-env locals-env)
  (cond
    ((equal (car expr) :CONST)
     expr)
    ((or (equal (car expr) :ARG) (equal (car expr) :VAR))
     (fp-get expr args-env locals-env))
    (t (error "~S is not a valid argument" expr))))

