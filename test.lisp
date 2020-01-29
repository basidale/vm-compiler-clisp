(require 'codecompute "./res/compute/compute.lisp")
(require 'codeadd "res/add.lisp")
(require 'vm "vm/vm.lisp")
(require 'compiler "compiler/compiler.lisp")

(defun test-add-5-3 ()
  (setq basecode '((defun add(x y) (+ x y)) (add 5 3)))
  (setq code (compile-code basecode))
  (print "Code : ")
  (print code)

  (setq vm (make-vm :name "vm" :memory-size 100 :stack-size 50))
  (vm-load code :vm vm)
  (setq result (vm-run :main nil :vm vm))

  (print "Result : ")
  (print result))

(test-add-5-3)

(defun test-compute-add-1-2 ()
  (setq code (compile-code compute-code))
  (print "Code : ")
  (print code)

  (setq vm (make-vm :name "vm" :memory-size 100 :stack-size 50))
  (vm-load code :vm vm)
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

(test-add-5-3)
;(print "1 + 2 = ")
;(test-compute-add-1-2)

;; (print "5 + 3 = ")
;; (test-compute-sub-5-3)
