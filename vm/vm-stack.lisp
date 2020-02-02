; Fix incremernt before

(defun vm-stack-push (vm src)
  (setf (vm-memory-at (vm-get-register vm 'SP) vm) src)
  (setf (vm-get-register vm 'SP) (+ 1 (vm-get-register vm 'SP))))

(defun vm-stack-pop (vm dest)
  (setf (vm-get-register vm 'SP) (- (vm-get-register vm 'SP) 1))
  (let ((stack-top (vm-memory-at (vm-get-register vm 'SP) vm)))
    (setf (vm-get-register vm dest) stack-top)
    stack-top))

(defun vm-fp-find (vm offset)
  (vm-memory-at (+ (vm-get-register vm 'FP) offset) vm))
