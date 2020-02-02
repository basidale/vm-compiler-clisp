					;TODO: use &rest and apply instead of funcall

(defun li2-to-vm-compile-expr (expr env)
  (cond
    ((equal (car expr) :CONST)
     expr)
    ((equal (car expr) :ARG)
     (compile-argument expr env))
    ((and (equal (car expr) :CALL) (is-arithmetic-expression (cdr expr)))
     (compile-arithmetic-expression (cadr expr) (li2-to-vm-map-compile-expr (cddr expr) env) env))
    ((equal (car expr) :CALL)
     (compile-function-call (cadr expr) (li2-to-vm-map-compile-expr (cddr expr) env)))))

(defun li2-to-vm-map-compile-expr (expr args)
  (map 'list
       (lambda (expr)
	 (li2-to-vm-compile-expr expr args))
       expr))

(defun li2-to-vm-compile-function (name args body)
  (append `((label ,name))
	  (apply #'append (li2-to-vm-map-compile-expr body args))
	  `((rtn))))

(defun li2-to-vm-jump-to-main ()
  '((JSR MAIN)(HALT)))

(defun compile-li2-to-vm (code)
  (append (li2-to-vm-jump-to-main)
	  (apply #'append (map 'list
			       (lambda (function)
				 (li2-to-vm-compile-function (car function) (cdadr function) (cdaddr function)))
			       code))))

(compile-li2-to-vm '((ADD (:ARGS A B) (:BODY (:CALL + (:ARG A) (:ARG B))))
		     (MAIN NIL (:BODY (:CALL ADD (:CONST 1) (:CONST 2))))))
