(defun compile-code (code)
  (let ((li1-code (compile-cl-to-li1 code)))
    (print li1-code)
    (let ((li2-code (compile-li1-to-li2 li1-code)))
      (print li2-code)
      (let ((vm-code (compile-li2-to-vm li2-code)))
	(print vm-code)
	vm-code))))




