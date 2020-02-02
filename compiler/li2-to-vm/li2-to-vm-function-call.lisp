(defun function-call-push-argument (arg)
  `(push ,arg))

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

(defun compile-function-call (name args)
  (append (map 'list #'function-call-push-argument args)
	  (function-call-store-and-update-fp)
	  (function-call-push-number-of-arguments args)
	  (function-call-push-old-fp)
	  (function-call-break name)
	  (function-call-reset-stack args)))


