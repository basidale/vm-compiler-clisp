(defun vm-stack-push (vm src)
  (setf (aref (vm-memory vm) (vm-get-register vm 'SP)) src)
  (setf (vm-get-register vm 'SP) (+ 1 (vm-get-register vm 'SP))))

(defun vm-stack-pop (vm dest)
  (setf (vm-get-register vm 'SP) (- (vm-get-register vm 'SP) 1))
  (setf (vm-get-register vm dest) (aref (vm-memory vm) (vm-get-register vm 'SP))))
