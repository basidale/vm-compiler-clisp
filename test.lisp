;; (require 'codecompute "./res/compute.lisp")
;; (require 'fact "./res/fact.lisp")
(require 'fact "./res/fibo.lisp")
(require 'init "./init.lisp")

(defun test-compile-run (basecode)
  (let ((code (compile-code basecode)))
    (print "Code : ")
    (print code)
    (test-run code)))

(defun test-run (code)
  (let ((vm (make-vm :name "vm" :memory-size 100 :stack-size 50)))
    (vm-load code :vm vm)
    (let ((result (vm-run :main nil :vm vm)))
      (print "Result : ")
      (print result))))

(compile-code (factsource 10))


;; (compile-code '((defun add(x y) (+ x y)) (add 5 3)))
;; (test-compile-run '((defun add(x y) (+ x y)) (add 5 3)))
;; (test-run (factvm 10))
;; (test-run codefibo)
;; (test-compile-run (compute-code 'add-op 1 2))
