					;TODO: main argument in vm-exec
					;TODO: Empty code in memory when vm-load
					;TODO: vm-exec : let(let) -> let*
					;TODO: Prevent exec unloaded code
					;TODO: Check stack size < memory and code < remaining space
					;TODO: Automatically halt when code-end reached
					;TODO: setf(aref) -> using macro

(require "vm-helper.lisp")
(require "vm-statements.lisp")

(defun vm-statements ()
  '((halt . vm-halt)
    (push . vm-push)))

(defun make-vm (&key name memory-size stack-size)
  `((vm-stack-size . ,stack-size)
    (vm-name . ,name)
    (vm-memory . ,(make-array memory-size))
    (vm-code-begin . 0)
    (vm-code-end . nil)
    (vm-stack-begin . nil)
    (vm-stack-end . nil)
    (vm-registers . ,(make-array 8 :initial-element 0))
    (vm-running . nil)))

(defun vm-load (code &key vm)
  (let ((index -1))
    (loop for stmt in code do
	  (progn
	    (setq index (+ index 1))
	    (setf (aref (vm-memory vm) index) stmt)))
    (rplacd (assoc 'vm-code-end vm) index)
    (rplacd (assoc 'vm-stack-begin vm) (+ index 1))
    (rplacd (assoc 'vm-stack-end vm) (+ (vm-stack-begin vm) (- (vm-stack-size vm) 1)))
    (setf (vm-bp vm) (vm-stack-begin vm))
    (setf (vm-sp vm) (vm-stack-begin vm))
    (setf (vm-fp vm) (vm-stack-begin vm)))
  t)

(defun vm-run (&key main vm)
  (rplacd (assoc 'vm-running vm) t)
  (loop while (is-vm-running vm) do
	(let ((next-pc (vm-exec (find-statement (vm-pc vm) :vm vm) :vm vm)))
	  (if next-pc
	      (setf (aref (vm-registers vm) 6) next-pc)))))

(defun vm-exec (stmt &key vm)
  (let ((verb (car stmt ))
	(args (cdr stmt)))
    (let ((callback (assoc verb (vm-statements))))
      (if callback
	  (apply (cdr callback) (list vm args))
	(error "~S is not implemented" verb)))))
  
