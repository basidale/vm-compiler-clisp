(defun vm-registers-mapping ()
  '((R0 . 0)
    (R1 . 1)
    (R2 . 2)
    (BP . 3)
    (SP . 4)
    (FP . 5)
    (PC . 6)
    (CMP . 7)))

(defun vm-registers (vm)
  (cdr (assoc 'vm-registers vm)))

(defmacro vm-get-register (vm register)
  `(aref (vm-registers ,vm) (vm-register-index ,register)))

(defun vm-register-index (register)
  (cdr (assoc register (vm-registers-mapping))))
