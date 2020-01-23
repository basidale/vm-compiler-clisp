(require "res/add.lisp")
(require "vm/vm.lisp")
(require "compiler/compiler.lisp")

(defun test-add-xy ()
  (setq code (compile-code '((defun add(x y) (+ x y)) (add 5 3))))
  (print "Code : ")
  (print code)

  (setq vm (make-vm :name "vm" :memory-size 100 :stack-size 50))
  (vm-load code :vm vm)
  (setq result (vm-run :main nil :vm vm))

  (print "Result : ")
  (print result))

(test-add-xy)

