(defun function-call-push-argument (arg)
  `(push (:const ,arg)))

(defun function-call-update-fp ()
  '((move SP FP)))

(defun function-call-push-number-of-arguments (args)
  `((push (:const ,(length args)))))

(defun function-call-break (label)
  `((jsr ,label)))

(defun function-call-reset-stack (args)
  `((add (:const ,(- -1 (length args))) SP)
    (add (:const ,(- 0 (length args))) FP)))

(defun compile-function-call (name args)
  (let ((target (append (map 'list #'function-call-push-argument args)
			(function-call-update-fp)
			(function-call-push-number-of-arguments args)
			(function-call-break name)
			(function-call-reset-stack args))))
    (make-result target)))


