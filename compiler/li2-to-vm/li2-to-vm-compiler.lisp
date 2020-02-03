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

(defun li2-to-vm-compile-expr (expr env src src-dest compiler)
  (cond
    ((equal (car expr) :CONST)
     expr)
    ((equal (car expr) :ARG)
     (compile-argument expr env))
    ((equal (car expr) :IF)
     (compile-condition (cadr expr) (cadddr expr) (caddr (cdddr expr)) env src src-dest compiler))
    ((is-comparison (expr))
     (compile-comparison (car expr) (cadr expr) (caddr expr) env compiler))
    ((is-arithmetic-expression expr)
     (compile-arithmetic-expression (car expr) (li2-to-vm-map-compile-expr (cdr expr) env src src-dest compiler) env src src-dest))
    ((equal (car expr) :CALL)
     (compile-function-call (cadr expr) (li2-to-vm-map-compile-expr (cddr expr) env src src-dest compiler)))))

(defun li2-to-vm-map-compile-expr (expr args src src-dest compiler)
  (map 'list
       (lambda (expr)
	 (li2-to-vm-compile-expr expr args src src-dest compiler))
       expr))

(defun li2-to-vm-compile-function (name args body compiler)
  (append `((label ,name))
	  (apply #'append (li2-to-vm-map-compile-expr body args 'R1 'R0 compiler))
	  `((rtn))))

(defun li2-to-vm-jump-to-main ()
  '((JSR MAIN)(HALT)))

(defun compile-li2-to-vm (code)
  (let ((compiler (make-compiler)))
	(append (li2-to-vm-jump-to-main)
	  (apply #'append (map 'list
			       (lambda (function)
				 (li2-to-vm-compile-function (car function) (cdadr function) (cdaddr function) compiler))
			       code)))))
