(setq compute-code '((defun add (x y)
		       (+ x y))

		     (defun sub (x y)
		       (- x y))

		     (defun mul (x y)
		       (* x y))

		     (defun div (x y)
		       (/ x y))

		     (defun compute (op x y)
		       (if (equal op 'add-op)
			   (add x y)
			   (if (equal op 'sub-op)
			       (sub x y)
			       (if (equal op 'mul-op)
				   (mul x y)
				   (if (equal op 'div-op)
				       (div x y)
				       -1)))))

		     (compute 'mul-op 3 7)))
