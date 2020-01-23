					;TODO: Check function exists

(defun function-call-push-argument (arg)
  `(push (:const ,arg)))

(defun function-call-update-fp ()
  '((move SP FP)))

(defun function-call-break (label)
  `((jsr ,label)))

(defun function-call-reset-stack (args)
  `((add (:const ,(- 1 (length args))) SP)
    (add (:const ,(- 0 (length args))) FP)))

(defun compile-function-call (name args instructions-list)
  (let ((target (append (map 'list #'function-call-push-argument args)
			(function-call-update-fp)
			(function-call-break name)
			(function-call-reset-stack args))))
    (setq instructions-list (append instructions-list target))))

(compile-function-call 'add '(1 2) '())
