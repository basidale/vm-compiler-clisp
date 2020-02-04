(defmacro vm-resolution-table-cell (vm)
  `(assoc 'vm-resolution-table ,vm))

(defun vm-resolution-table (vm)
  (cdr (vm-resolution-table-cell vm)))

(defun vm-add-to-resolution-table (vm label address)
  (setf (cdr (vm-resolution-table-cell vm)) (cons (cons label address) (vm-resolution-table vm))))

(defun vm-resolve-address (vm label)
  (cdr (assoc label (vm-resolution-table vm))))

(defun vm-resolve-variable-at (vm label)
  (let ((value (vm-resolve-address vm label)))
    (if (and (consp value) (equal (car value) ':const))
	(cadr value)
	(error "Label ~S doesn't point unto a global variable" label))))
