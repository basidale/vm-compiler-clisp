(defmacro vm-memory-cell (vm)
  `(assoc 'vm-memory ,vm))

(defun vm-memory (vm)
  (cdr (vm-memory-cell vm)))

(defmacro vm-memory-at (address vm)
  `(aref (vm-memory ,vm) ,address))
