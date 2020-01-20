(defmacro vm-running-cell (vm)
  `(assoc 'vm-running ,vm))

(defun vm-stack-size (vm)
  (cdr (assoc 'vm-stack-size vm)))

(defun vm-memory (vm)
  (cdr (assoc 'vm-memory vm)))

(defun is-vm-running (vm)
  (cdr (vm-running-cell vm)))

(defun vm-fp-find (vm offset)
  (aref (vm-memory vm) (+ (vm-get-register vm 'FP) offset)))
