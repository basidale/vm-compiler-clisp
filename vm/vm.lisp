					;TODO: Check decrement before pop
					;TODO: Add move with register
					;TODO:  Remove code-begin/end stack-begin/end if unused
					;TODO: Try rplacd -> setf
					;TODO: main argument in vm-exec
					;TODO: Empty code in memory when vm-load
					;TODO: vm-exec : let(let) -> let*
					;TODO: Prevent exec unloaded code
					;TODO: Check stack size < memory and code < remaining space
					;TODO: Automatically halt when code-end reached
					;TODO: setf(aref) -> using macro
					;TODO: Resolution table -> hashtable
					;TODO: maybe add (pop nil)
(require "vm-address-resolution.lisp")
(require "vm-memory.lisp")
(require "vm-registers.lisp")
(require "vm-stack.lisp")
(require "vm-statements.lisp")

(defun make-vm (&key name memory-size stack-size)
  `((vm- stack-size . ,stack-size)
    (vm-name . ,name)
    (vm-memory . ,(make-array memory-size))
    (vm-registers . ,(make-array 8 :initial-element 0))
    (vm-running . nil)
    (vm-resolution-table . nil)))

(defun vm-load (code &key vm)
  (let ((index -1))
    (loop for stmt in code do
	  (progn
	    (setq index (+ index 1))
	    (setf (vm-memory-at index vm) stmt)
	    (if (equal (car stmt) 'label)
		(vm-add-to-resolution-table vm (cadr stmt) index)
	      nil)))
    (let ((stack-begin (+ index 1)))
      (setf (vm-get-register vm 'BP) stack-begin)
      (setf (vm-get-register vm 'SP) stack-begin)
      (setf (vm-get-register vm 'FP) stack-begin)))
  t)

(defun vm-run (&key main vm)
  (rplacd (vm-running-cell vm) t)
  (loop while (is-vm-running vm) do
	(progn
	  (let ((next-pc (vm-exec (find-statement (vm-get-register vm 'PC) :vm vm) :vm vm)))
	    (if next-pc
		(setf (aref (vm-registers vm) 6) next-pc)))))
  (vm-get-register vm 'R0))

(defun vm-exec (stmt &key vm)
  (let ((verb (car stmt ))
	(args (cdr stmt)))
    (let ((callback (assoc verb (vm-statements))))
      (if callback
	  (apply (cdr callback) (list vm args))
	(error "~S is not implemented" verb)))))
  
(defmacro vm-running-cell (vm)
  `(assoc 'vm-running ,vm))

(defun is-vm-running (vm)
  (cdr (vm-running-cell vm)))
