
(defun cl-to-li1-compile-function-definition (expr args locals)
  `(:function ,(cadr expr) (:args ,@(caddr expr)) (:body ,@(cl-to-li1-map-compile (cdddr expr) (append args (caddr expr)) locals))))

(defun cl-to-li1-compile-condition (expr args locals)
  `(:if ,(cl-to-li1-compile-expr (cadr expr) args locals) :then ,(cl-to-li1-compile-expr (caddr expr) args locals) :else ,(cl-to-li1-compile-expr (cadddr expr) args locals)))

					;TODO: Separate case for +-*/
(defun cl-to-li1-compile-function-call (expr args locals)
  `(:call ,(car expr) ,@(cl-to-li1-map-compile (cdr expr) args locals)))

(defun cl-to-li1-compile-constant (expr)
  `(:const ,expr))

(defun cl-to-li1-compile-argument (expr)
  `(:arg ,expr))

(defun cl-to-li1-compile-variable (expr)
  `(:var ,expr))

					;TODO: See how implemented in final phase
(defun cl-to-li1-arithmetic-operator (expr)
  (assoc (car expr) '((+ . :add)
		      (- . :sub)
		      (* . :mul)
		      (/ . :div))))

(defun cl-to-li1-is-arithmetic-expression (expr)
  (if (cl-to-li1-arithmetic-operator expr) t nil))

(defun cl-to-li1-compile-arithmetic-expression (expr args locals)
  `(,(cdr (cl-to-li1-arithmetic-operator expr)) ,@(cl-to-li1-map-compile (cdr expr) args locals)))

(defun cl-to-li1-comparison-operator (expr)
  (assoc (car expr) '((<  . :lt)
		      (<= . :le)
		      (=  . :eq)
		      (>  . :ge)
		      (>= . :ht))))

(defun cl-to-li1-is-comparison (expr)
  (if (cl-to-li1-comparison-operator expr) t nil))

(defun cl-to-li1-compile-comparison (expr args locals)
  `(,(cdr (cl-to-li1-comparison-operator expr)) ,@(cl-to-li1-map-compile (cdr expr) args locals)))

(defun cl-to-li1-compile-let (bindings expr args locals)
  (labels ((find-symbols (bindings)
	     (if (null bindings)
		 nil
		 (cons (caar bindings) (find-symbols (cdr bindings)))))
	   (compile-bindings (bindings)
	     (if (null bindings)
		 nil
		 (append `((,(caar bindings) ,(cl-to-li1-compile-expr (cadar bindings) args locals)))
		     (compile-bindings (cdr bindings))))))
    `(:let ,(compile-bindings bindings)
       ,(cl-to-li1-compile-expr expr args (append locals (find-symbols bindings))))))

(defun cl-to-li1-compile-expr (expr args locals)
  (if (atom expr)
      (if (constantp expr)
	  (cl-to-li1-compile-constant expr)
	  (if (symbolp expr)
	      (if (member expr args)
		  (cl-to-li1-compile-argument expr)
		  (if (member expr locals)
		      (cl-to-li1-compile-variable expr)
		      (error "Unbound symbol ~S" expr)))
	      (error "Atomic non-constant found")))
      (if (eq (car expr) 'defun)
	  (cl-to-li1-compile-function-definition expr args locals)
	  (if (eq (car expr) 'if)
	      (cl-to-li1-compile-condition expr args locals)
	      (if (cl-to-li1-is-arithmetic-expression expr)
		  (cl-to-li1-compile-arithmetic-expression expr args locals)
		  (if (cl-to-li1-is-comparison expr)
		      (cl-to-li1-compile-comparison expr args locals)
		      (if (equal (car expr) 'let)
			  (cl-to-li1-compile-let (cadr expr) (caddr expr) args locals)
			  (cl-to-li1-compile-function-call expr args locals))))))))

(defun cl-to-li1-map-compile (expr args locals)
  (map 'list
       (lambda (e)
	 (cl-to-li1-compile-expr e args locals))
       expr))

(defun compile-cl-to-li1 (code)
  (cl-to-li1-map-compile code nil nil))

