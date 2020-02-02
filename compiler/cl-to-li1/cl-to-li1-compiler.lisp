
(defun cl-to-li1-compile-function-definition (expr args)
  `(:function ,(cadr expr) (:args ,@(caddr expr)) (:body ,@(cl-to-li1-map-compile (cdddr expr) (append args (caddr expr))))))

(defun cl-to-li1-compile-condition (expr args)
  `(:if ,(cl-to-li1-compile-expr (cadr expr) args) :then ,(cl-to-li1-compile-expr (caddr expr) args) :else ,(cl-to-li1-compile-expr (cadddr expr) args)))

(defun cl-to-li1-compile-function-call (expr args)
  `(:call ,(car expr) ,@(cl-to-li1-map-compile (cdr expr) expr)))

(defun cl-to-li1-compile-constant (expr)
  `(:const ,expr))

(defun cl-to-li1-compile-argument (expr)
  `(:arg ,expr))

(defun cl-to-li1-compile-variable (expr)
  `(:var ,expr))

(defun cl-to-li1-compile-expr (expr args)
  (if (atom expr)
      (if (constantp expr)
	  (cl-to-li1-compile-constant expr)
	  (if (symbolp expr)
	      (if (member expr args)
		  (cl-to-li1-compile-argument expr)
		  (cl-to-li1-compile-variable expr))
	      (error "Atomic non-constant found")))
      (if (eq (car expr) 'defun)
	  (cl-to-li1-compile-function-definition expr args)
	  (if (eq (car expr) 'if)
	      (cl-to-li1-compile-condition expr args)
	      (cl-to-li1-compile-function-call expr args)))))

(defun cl-to-li1-map-compile (expr args)
  (map 'list
       (lambda (e)
	 (cl-to-li1-compile-expr e args))
       expr))

(defun compile-cl-to-li1 (code)
  (cl-to-li1-map-compile code nil))

;; (compile-cl-to-li1 '((defun add (a b) (+ a b)) (add 1 2)))

