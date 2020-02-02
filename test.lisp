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

;; (test-compile-run '((defun add(x y) (+ x y)) (add 5 3)))
;; (test-run codefact)
(test-run codefibo)
;; (test-compile-run (compute-code 'add-op 1 2))

;; (defun test-compute-sub-5-3 ()
;;   (print "Code : ")
;;   (print codecompute)

;;   (setq vm (make-vm :name "vm" :memory-size 100 :stack-size 50))
;;   (vm-load codecompute-sub-5-3 :vm vm)
;;   (setq result (vm-run :main nil :vm vm))

;;   (print "Result : ")
;;   (print result))

;(print "1 + 2 = ")
;(test-compute-add-1-2)

;; (print "5 + 3 = ")
;; (test-compute-sub-5-3)
