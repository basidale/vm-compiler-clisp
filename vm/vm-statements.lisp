(defun vm-statements-mapping ()
  '((halt  . vm-halt)
    (push  . vm-push)
    (pop   . vm-pop)
    (move  . vm-move)
    (jsr   . vm-jsr)
    (label . vm-label)
    (add   . vm-add)
    (rtn   . vm-rtn)))

(defun find-statement-callback (verb)
  (assoc verb (vm-statements-mapping)))

(defun find-statement (pc &key vm)
  (vm-memory-at pc vm))

(defun vm-halt (vm args)
  (rplacd (assoc 'vm-running vm) nil)
  nil)

(defun vm-push (vm args)
  (let ((src (src-dispatch vm (car args))))
    (vm-stack-push vm src))
  (increment-program-counter vm))

(defun vm-pop (vm args)
  (let ((dest (car args)))
    (vm-stack-pop vm dest))
  (increment-program-counter vm))

(defun vm-move (vm args)
  (let ((src (src-dispatch vm (car args)))
	(dest (cadr args)))
    (setf (vm-get-register vm dest) src)
  (increment-program-counter vm)))

(defun vm-add (vm args)
  (let ((src (car args))
	(dest (cadr args)))
    (setq src (src-dispatch vm src))
    (if (symbolp src)
	(setf (vm-get-register vm dest) (+ (vm-get-register vm src) (vm-get-register vm dest)))
      (setf (vm-get-register vm dest) (+ src (vm-get-register vm dest)))))
  (increment-program-counter vm))

(defun vm-jsr (vm args)
  (let ((label (car args)))
    (vm-stack-push vm (+ (vm-get-register vm 'PC) 1))
    (vm-resolve-address vm label)))

(defun vm-rtn (vm args)
  (vm-stack-pop vm 'PC)
  (vm-get-register vm 'PC))

(defun vm-label (vm args)
  (increment-program-counter vm))

(defun src-dispatch (vm src)
  (cond
   ((and (consp src) (equal (car src) 'FP)) (vm-fp-find vm (cadr src)))
   ((and (consp src) (equal (car src) ':const)) (cadr src))
   ((symbolp src) (vm-get-register vm src))))

(defun increment-program-counter (vm)
  (+ (vm-get-register vm 'PC)  1))
