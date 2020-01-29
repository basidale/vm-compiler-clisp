(defun compiler-get-function-definition (results)
  (cdr (assoc 'functions-definitions results)))

(defun compiler-get-instructions-list (results)
  (cdr (assoc 'instructions-list results)))
