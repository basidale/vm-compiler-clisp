(defmacro vm-code-end-cell (vm)
  `(assoc 'vm-code-end ,vm))

(defmacro vm-stack-begin-cell (vm)
  `(assoc 'vm-stack-begin ,vm))

(defmacro vm-stack-end-cell (vm)
  `(assoc 'vm-stack-end ,vm))

(defmacro vm-running-cell (vm)
  `(assoc 'vm-running ,vm))

(defun vm-stack-size (vm)
  (cdr (assoc 'vm-stack-size vm)))

(defun vm-memory (vm)
  (cdr (assoc 'vm-memory vm)))

(defun vm-code-begin (vm)
  (cdr (assoc 'vm-code-begin vm)))

(defun vm-code-end (vm)
  (cdr (vm-code-end-cell vm)))

(defun vm-stack-begin (vm)
  (cdr (vm-stack-begin-cell vm)))

(defun vm-stack-end (vm)
  (cdr (vm-stack-end-cell vm)))

(defun vm-registers (vm)
  (cdr (assoc 'vm-registers vm)))

(defmacro vm-get-register (vm register)
  `(aref (vm-registers ,vm) (vm-register-index ,register)))

(defun is-vm-running (vm)
  (cdr (vm-running-cell vm)))

(defun find-statement (pc &key vm)
  (aref (cdr (assoc 'vm-memory vm)) pc))

(defun vm-register-index (register)
  (cdr (assoc register (vm-registers-mapping))))
