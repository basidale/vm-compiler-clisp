(defun function-header (name)
  `((label ,name)))

(defun function-body (args body)
  (let ((target nil))
    (loop for stmt in body do
      (setq target (append target (result-code (compile-expr stmt args)))))
    target))

(defun function-end ()
  '((rtn)))

(defun compile-defun (name args body)
  (let ((target (append (function-header name)
			(function-body args body)
			(function-end))))
    (make-result target :function-definition t)))

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
