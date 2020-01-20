(defun vm-halt (vm args)
  (rplacd (assoc 'vm-running vm) nil)
  nil)

(defun vm-push (vm args)
  (let ((src (car args)))
    (setf (aref (vm-memory vm) (vm-sp vm)) (cadr src))
    (setf (aref (vm-registers vm) 4) (+ (vm-sp vm) 1)))
  (+ (vm-pc vm) 1))

(defun vm-move (vm args)
  (let ((src (car args))
	(dest (cadr args)))
    nil))
