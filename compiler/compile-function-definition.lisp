(defun function-definition-header (name)
  `((label ,name)))

(defun args-mapping (env)
  (let ((index (length env)))
    (map 'list
	 (lambda (arg)
	   (let ((pair `(,arg . (fp ,(- index)))))
	     (setq index (- index 1))
	     pair))
	 env)))

(defun args-get-mapping (identifier env)
  (cdr (assoc identifier (args-mapping env))))



					;TODO: Allow defun inside defun
(defun function-definition-body (args body)
  (let ((instructions-list nil))
    (loop for stmt in body do
	 (print (compile-expr stmt nil instructions-list))
	 (setq instructions-list (append instructions-list (compile-expr stmt args nil instructions-list))))
    instructions-list))

(defun compile-defun (name args body)
  (let ((target (append (function-definition-header name)
			(function-definition-body args body))))
    target))
;;(setq functions-definitions (append functions-definitions target))))

    (loop for stmt in code do
	 (compile-expr stmt functions-definitions instructions-list))
    nil))
