(setq codeadd '((push (:const 4))
		(move SP FP)
		(push (:const 1))
		(jsr facto)
		(add (:const -1) FP)
		(add (:const -2) SP)
		(halt)
		
		(label facto)
		(move (fp -1) R1)
		
		(cmp R0 1)
		(jleq endvalue)
		
		(move R0 R1)
		(add -1 R1)

		;; call for val > 1
		(push R1)
		(move SP FP)
		(push (:const 1))
		(jsr add)
		(add (:const -1) FP)
		(add (:const -2) SP)	
	
		(mul R1 R0)								
		(rtn)		

		;; handle val 1
		(label end)
		(mov 1 R0)	
				
		(rtn)))
