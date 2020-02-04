;; (require 'codecompute "./res/compute.lisp")
(require 'fact "./res/fact.lisp")
(require 'fact "./res/fibo.lisp")
(require 'init "./init.lisp")

(defun test-compile-run (basecode)
  (let ((code (compile-code basecode)))
    (print "Code : ")
    (print code)
    (test-run code)))

(defun test-run (code)
  (let ((vm (make-vm :name "vm" :memory-size 1000 :stack-size 200)))
    (vm-load code :vm vm)
    (let ((result (vm-run :main nil :vm vm)))
      (print "Result : ")
      (print result))))

(test-compile-run (factsource 3))

(compile-li1-to-li2 (compile-cl-to-li1 (factsource 10)))

(compile-cl-to-li1 (factsource 10))

;; (test-compile-run '((defun add(x y) (+ x y)) (add 5 3)))
;; (test-run (factvm 10))
;; (test-run codefibo)
;; (test-compile-run (compute-code 'add-op 1 2))

815915283247897734345611269596115894272000000000
815915283247897734345611269596115894272000000000 
