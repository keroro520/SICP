
;; 习题2.35
;; 书上说Horner方法是多项式的求值的最优算法
;; 即An*x^n + An-1*x^(n-1) + ... + A1*x + A0 
;;	=(...(An*x+An-1)*x + ... + A1)*x + A0
(define (accumulate op initial sequence)
	(if (null? sequence)
		initial
		(op (car sequence)
			(accumulate op initial (cdr sequence)))))


(define (horner-eval x coefficient-sequence)
	(accumulate 
		(lambda (this-coeff higher-terms) (+ this-coeff (* x higher-terms) ))
		0
		coefficient-sequence))

(horner-eval 2 (list 1 3 0 5 0 1))

(horner-eval 2 (list 2 3 0 5 0 1))
