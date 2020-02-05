(require 'test "test.lisp")

(defun menu ()
  (print "Make a choice (1: factorial - 2: fibonacci) : ")
  (let ((choice (read)))
    (print "Enter value : ")
    (let ((value (read)))
      (cond
	((equal choice 1)
	 (compile-run-fact value))
	((equal choice 2)
	 (compile-run-fibo value))
	(t (error "Invalid choice"))))))

(loop while t do
     (menu))
