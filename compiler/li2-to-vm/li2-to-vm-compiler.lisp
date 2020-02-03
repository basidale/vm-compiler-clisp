					;TODO: Normalize names
					;TODO: use &rest and apply instead of funcall
					;TODO: Add dest register parameter to compile functions

					;TODO: Generalize cmp
;; (defun compile-comparison (condition src src-dest)
;;   (append ()
;; 	  (li2-to-vm-compile-expr)))

;; (defun compile-condition (condition then else)
;;   (append (compile-comparison condition)))

(defun li2-to-vm-compile-expr (expr env src src-dest)
  (cond
    ((equal (car expr) :CONST)
     expr)
    ((equal (car expr) :ARG)
     (compile-argument expr env))
    ((equal (car expr) :IF)
     (compile-condition (cadr expr) (caddr expr) (caddddr expr)))
    ((is-arithmetic-expression expr)
     (compile-arithmetic-expression (car expr) (li2-to-vm-map-compile-expr (cdr expr) env src src-dest) env src src-dest))
    ((equal (car expr) :CALL)
     (compile-function-call (cadr expr) (li2-to-vm-map-compile-expr (cddr expr) env src src-dest)))))

(defun li2-to-vm-map-compile-expr (expr args src src-dest)
  (map 'list
       (lambda (expr)
	 (li2-to-vm-compile-expr expr args src src-dest))
       expr))

(defun li2-to-vm-compile-function (name args body)
  (append `((label ,name))
	  (apply #'append (li2-to-vm-map-compile-expr body args 'R1 'R0))
	  `((rtn))))

(defun li2-to-vm-jump-to-main ()
  '((JSR MAIN)(HALT)))

(defun compile-li2-to-vm (code)
  (append (li2-to-vm-jump-to-main)
	  (apply #'append (map 'list
			       (lambda (function)
				 (li2-to-vm-compile-function (car function) (cdadr function) (cdaddr function)))
			       code))))
