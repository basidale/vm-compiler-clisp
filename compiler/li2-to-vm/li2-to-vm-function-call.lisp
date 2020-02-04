(defun function-call-store-and-update-fp ()
  '((move FP R1)
    (move SP FP)))

(defun function-call-push-number-of-arguments (args)
  `((push (:const ,(length args)))))

(defun function-call-push-old-fp ()
  '((push R1)))

(defun function-call-break (label)
  `((jsr ,label)))

(defun function-call-reset-stack (args)
  `((pop FP)
    (add (:const ,(- -1 (length args))) SP)))

(defun function-call-push-arguments (args env compiler)
  (if (null args)
      nil
      (append (li2-to-vm-compile-expr (car args) env compiler)
	  (list(list 'push 'R0))
	  (function-call-push-arguments (cdr args) env compiler))))

(defun compile-function-call (name args env compiler)
  (append (function-call-push-arguments args env compiler)
	  (function-call-store-and-update-fp)
	  (function-call-push-number-of-arguments args)
	  (function-call-push-old-fp)
	  (function-call-break name)
	  (function-call-reset-stack args)))


