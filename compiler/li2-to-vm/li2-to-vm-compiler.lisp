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
    (setf (cdr (compiler-label-counter-cell compiler)) (+ label 1))
    label))

(defun compile-let (bindings body args-env locals-env compiler)
  (labels ((find-symbols (bindings)
	     (if (null bindings)
		 nil
		 (cons (caar bindings) (find-symbols (cdr bindings)))))
	   (compile-bindings (bindings)
	     (if (null bindings)
		 nil
		 (append (li2-to-vm-compile-expr (cadar bindings) args-env locals-env compiler)
		     '((push R0))
		     (compile-bindings (cdr bindings))))))
    (append (compile-bindings bindings)
	    (li2-to-vm-compile-expr body args-env (append locals-env (find-symbols bindings)) compiler)
	    `((add (:const ,(- (length bindings))) SP)))))

(defun li2-to-vm-compile-expr (expr args-env locals-env compiler)
  (cond
    ((or (equal (car expr) :CONST) (equal (car expr) :ARG) (equal (car expr) :VAR))
     (list (list 'move (compile-argument expr args-env locals-env) 'R0)))
    ((equal (car expr) :IF)
     (compile-condition (cadr expr) (cadddr expr) (caddr (cdddr expr)) args-env locals-env compiler))
    ((is-comparison expr)
     (compile-comparison (car expr) (cadr expr) (caddr expr) args-env locals-env compiler))
    ((is-arithmetic-expression expr)
     (compile-arithmetic-expression (car expr) (cdr expr) args-env locals-env compiler))
    ((equal (car expr) :LET)
     (compile-let (cadr expr) (caddr expr) args-env locals-env compiler))
    ((equal (car expr) :CALL)
       (compile-function-call (cadr expr) (cddr expr) args-env locals-env compiler))
    (t (error "Uncompilable expression ~S" expr))))

(defun li2-to-vm-map-compile-expr (expr args-env locals-env compiler)
  (append (li2-to-vm-compile-expr (car expr) args-env locals-env compiler)
	  (li2-to-vm-map-compile-expr (cdr expr) args-env locals-env compiler)))

(defun li2-to-vm-compile-function (function compiler)
  (let ((name (car function))
	(args (cadr function))
	(body (cdaddr function)))
    (labels ((recurs (code)
	     (if (null code)
		 nil
		 (append (li2-to-vm-compile-expr (car code) args nil compiler)
			 (recurs (cdr code))))))
      (append `((label ,name))
	      (recurs body)
	      '((rtn))))))

(defun li2-to-vm-jump-to-main ()
  '((JSR MAIN)(HALT)))

(defun compile-li2-to-vm (code)
  (let ((compiler (list (cons 'label-counter 0))))
    (labels ((recurs (code)
	       (if (null code)
		   nil
		   (append (li2-to-vm-compile-function (car code) compiler)
			   (recurs (cdr code))))))
      (append (li2-to-vm-jump-to-main)
	      (recurs code)))))
