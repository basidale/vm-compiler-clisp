(defun function-definition-header (name)
  `((label ,name)))

(defun function-definition-body (args body)
  (let ((instructions-list nil))
    (loop for stmt in body do
	 (setq instructions-list (append instructions-list (compile-expr stmt args nil instructions-list))))
    instructions-list))

(defun function-definition-end ()
  '((rtn)))

(defun compile-defun (name args body)
  (append (function-definition-header name)
	  (function-definition-body args body)
	  (function-definition-end)))

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
