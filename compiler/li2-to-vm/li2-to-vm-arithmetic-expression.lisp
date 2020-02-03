					;TODO: One function for 4 operations

(defun compile-arithmetic-operation-set-registers (args env src src-dest)
  `((move ,(car args) ,src)
    (move ,(cadr args) ,src-dest)))

(defun compile-add(args env src src-dest)
  (append (compile-arithmetic-operation-set-registers args env src src-dest)
	  `((add ,src ,src-dest))))

(defun compile-sub(args env src src-dest)
  (append (compile-arithmetic-operation-set-registers args env src src-dest)
	  `((sub ,src ,src-dest))))

(defun compile-mul(args env src src-dest)
  (append (compile-arithmetic-operation-set-registers args env src src-dest)
	  `((mul ,src ,src-dest))))

(defun compile-div(args env src src-dest)
  (append (compile-arithmetic-operation-set-registers args env src src-dest)
	  `((div ,src ,src-dest))))

(defun arithmetic-compiler (operator)
  (cdr (assoc operator (arithmetic-operators))))

(defun is-arithmetic-expression (expr)
  (not (null (assoc (car expr) (arithmetic-operators)))))

(defun compile-arithmetic-expression (operator args env src src-dest)
  (funcall (arithmetic-compiler operator) args env src src-dest))

(defun arithmetic-operators ()
  '((:add . compile-add)
    (:sub . compile-sub)
    (:mul . compile-mul)
    (:div . compile-div)))
