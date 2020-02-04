					;TODO: Normalize names
					;TODO: use &rest and apply instead of funcall
					;TODO: Add dest register parameter to compile functions
					;TODO: Generalize cmp
					;TODO: is-constant, is-arg, etc.
(defun make-compiler ()
  '((label-counter . 0)))

(defmacro compiler-label-counter-cell (compiler)
  `(assoc 'label-counter ,compiler))

(defun compiler-label-counter (compiler)
  (cdr (compiler-label-counter-cell compiler)))

(defun compiler-increment-label-counter (compiler)
  (let ((label (compiler-label-counter compiler)))
    (rplacd (compiler-label-counter-cell compiler) (+ label 1))
    label))

(defun li2-to-vm-compile-expr (expr env compiler)
  (cond
;;    ((equal (car expr) :IF)
;;     (compile-condition (cadr expr) (cadddr expr) (caddr (cdddr expr)) env compiler))
;;    ((is-comparison (expr))
;;     (compile-comparison (car expr) (cadr expr) (caddr expr) env compiler))
    ((is-arithmetic-expression expr)
     (compile-arithmetic-expression (car expr) (map-compile-argument (cdr expr) env) env))
    ((equal (car expr) :CALL)
     (compile-function-call (cadr expr) (map-compile-argument (cddr expr) env)))
    (t (error "Uncompilable expression ~S" expr))))

(defun li2-to-vm-map-compile-expr (expr env compiler)
  (append (li2-to-vm-compile-expr (car expr) env compiler)
	  (li2-to-vm-map-compile-expr (cdr expr) env compiler)))

(defun li2-to-vm-compile-function (function compiler)
  (let ((name (car function))
	(args (cadr function))
	(body (cdaddr function)))
    (labels ((recurs (code)
	     (if (null code)
		 nil
		 (append (li2-to-vm-compile-expr (car code) args compiler)
			 (recurs (cdr code))))))
      (append `((label ,name))
	      (recurs body)
	      '((rtn))))))

(defun li2-to-vm-jump-to-main ()
  '((JSR MAIN)(HALT)))

(defun compile-li2-to-vm (code)
  (let ((compiler (make-compiler)))
    (labels ((recurs (code)
	       (if (null code)
		   nil
		   (append (li2-to-vm-compile-function (car code) compiler)
			   (recurs (cdr code))))))
      (append (li2-to-vm-jump-to-main)
	      (recurs code)))))
