(defmacro vm-running-cell (vm)
  `(assoc 'vm-running ,vm))

(defun vm-stack-size (vm)
  (cdr (assoc 'vm-stack-size vm)))

(defun vm-memory (vm)
  (cdr (assoc 'vm-memory vm)))

(defun vm-registers (vm)
  (cdr (assoc 'vm-registers vm)))

(defmacro vm-get-register (vm register)
  `(aref (vm-registers ,vm) (vm-register-index ,register)))

(defun is-vm-running (vm)
  (cdr (vm-running-cell vm)))

(defun vm-register-index (register)
  (cdr (assoc register (vm-registers-mapping))))
