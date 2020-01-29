					;TODO: load-stmt -> load

(defun vm-statements-mapping ()
  '((halt  . vm-halt)
    (push  . vm-push)
    (pop   . vm-pop)
    (move  . vm-move)
    (jmp   . vm-jmp)
    (jsr   . vm-jsr)
    (add   . vm-add)
    (sub   . vm-sub)
    (rtn   . vm-rtn)
    (load  . vm-load-stmt)
    (cmp . vm-cmp)
    (jeq . vm-jeq)
    ;; Non-executable
    (label . vm-label)))

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
  (let ((src (src-dispatch vm (car args)))
	(dest (cadr args)))
    (setf (vm-get-register vm dest) (+ src (vm-get-register vm dest))))
  (increment-program-counter vm))

(defun vm-sub (vm args)
  (let ((src (src-dispatch vm (car args)))
	(dest (cadr args)))
    (setf (vm-get-register vm dest) (- src (vm-get-register vm dest))))
  (increment-program-counter vm))

(defun vm-jsr (vm args)
  (let ((label (car args)))
    (vm-stack-push vm (+ (vm-get-register vm 'PC) 1))
    (vm-resolve-address vm label)))

(defun vm-jmp (vm args)
  (let ((label (car args)))
    (vm-resolve-address vm label)))

(defun vm-rtn (vm args)
  (vm-stack-pop vm 'PC)
  (vm-get-register vm 'PC))

(defun vm-load-stmt (vm args)
  (let ((src  (src-dispatch vm (car args)))
	(dest (cadr args)))
    (setf (vm-get-register vm dest) src))
  (increment-program-counter vm))

(defun cmp-flag-value (flag)
  (if (null flag) 0 1))

(defun vm-cmp (vm args)
  (let ((src1 (src-dispatch vm (car args)))
	(src2 (src-dispatch vm (cadr args))))
    (let ((ltflag (cmp-flag-value (< src1 src2)))
	  (eqflag (cmp-flag-value (= src1 src2)))
	  (gtflag (cmp-flag-value (> src1 src2))))
      (setf (vm-get-register vm 'CMP) 0)
      (setf (vm-get-register vm 'CMP) (logior ltflag (ash eqflag 1) (ash gtflag 2)))))
  (increment-program-counter vm))

(defun vm-cmp-equal (vm)
  (if (> (logand (vm-get-register vm 'CMP) #b010) 0) t nil))

(defun vm-jeq (vm args)
  (let ((cmp-equal (vm-cmp-equal vm))
	(label (car args)))
    (if cmp-equal (vm-resolve-address vm label) (increment-program-counter vm))))

(defun vm-cmp-less-than (vm)
  (if (> (logand (vm-get-register vm 'CMP) #b100) 0) t nil))

(defun vm-jlt (vm args)
  (let ((cmp-less-than (vm-cmp-less-than vm))
	(label (car args)))
    (if cmp-less-than (vm-resolve-address vm label) (increment-program-counter vm))))

(defun vm-cmp-less-equal (vm)
  (if (> (logand (vm-get-register vm 'CMP) #b110) 0) t nil))

(defun vm-jle (vm args)
  (let ((cmp-less-equal (vm-cmp-less-than vm))
	(label (car args)))
    (if cmp-less-equal (vm-resolve-address vm label) (increment-program-counter vm))))

(defun vm-cmp-greater-than (vm)
  (if (> (logand (vm-get-register vm 'CMP) #b001) 0) t nil))

(defun vm-jgt (vm args)
  (let ((cmp-greater-than (vm-cmp-greater-than vm))
	(label (car args)))
    (if cmp-greater-than (vm-resolve-address vm label) (increment-program-counter vm))))

(defun vm-cmp-greater-equal (vm)
  (if (> (logand (vm-get-register vm 'CMP) #b011) 0) t nil))

(defun vm-jge (vm args)
  (let ((cmp-greater-equal (vm-cmp-greater-equal vm))
	(label (car args)))
    (if cmp-greater-equal (vm-resolve-address vm label) (increment-program-counter vm))))

(defun vm-cmp-not-equal (vm)
  (if (> (logand (vm-get-register vm 'CMP) #b101) 0) t nil))

(defun vm-jne (vm args)
  (let ((cmp-not-equal (vm-cmp-not-equal vm))
	(label (car args)))
    (if cmp-not-equal (vm-resolve-address vm label) (increment-program-counter vm))))

(defun src-dispatch (vm src)
  (cond
   ((and (consp src) (equal (car src) 'FP)) (vm-fp-find vm (cadr src)))
   ((and (consp src) (equal (car src) ':const)) (cadr src))
   ((and (consp src) (equal (car src) ':ref)) (vm-resolve-variable vm label))
   ((symbolp src) (vm-get-register vm src))))

(defun increment-program-counter (vm)
  (+ (vm-get-register vm 'PC)  1))

(defun vm-label (vm args)
  (increment-program-counter vm))
