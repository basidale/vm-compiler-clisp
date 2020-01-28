(require "res/vmcompute.lisp")
(require "res/add.lisp")
(require "vm/vm.lisp")
(require "compiler/compiler.lisp")

(defun test-add-xy ()
  (setq code (compile-code '((defun add(x y) (+ x y)) (add 5 3))))
  (print "Code : ")
  (print code)

  (setq vm (make-vm :name "vm" :memory-size 100 :stack-size 50))
  (vm-load codeadd :vm vm)
  (setq result (vm-run :main nil :vm vm))
<
  (print "Result : ")
  (print result))

(defun test-compute-add-1-2 ()
  (print "Code : ")
  (print codecompute)

  (setq vm (make-vm :name "vm" :memory-size 100 :stack-size 50))
  (vm-load codecompute-add-1-2 :vm vm)
  (setq result (vm-run :main nil :vm vm))

  (print "Result : ")
  (print result))

(defun test-compute-sub-5-3 ()
  (print "Code : ")
  (print codecompute)

  (setq vm (make-vm :name "vm" :memory-size 100 :stack-size 50))
  (vm-load codecompute-sub-5-3 :vm vm)
  (setq result (vm-run :main nil :vm vm))

  (print "Result : ")
  (print result))

(print "1 + 2 = ")
(test-compute-add-1-2)

(print "5 + 3 = ")
(test-compute-sub-5-3)
