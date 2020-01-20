(defmacro vm-resolution-table-cell (vm)
  `(assoc 'vm-resolution-table ,vm))

(defun vm-resolution-table (vm)
  (cdr (vm-resolution-table-cell vm)))

(defun vm-add-to-resolution-table (vm label address)
  (rplacd (vm-resolution-table-cell vm) (cons (cons label address) (vm-resolution-table vm))))

(defun vm-resolve-address (vm label)
  (cdr (assoc label (vm-resolution-table vm))))
