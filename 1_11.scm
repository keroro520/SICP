; 习题1.11
;f(n) = n ,  if n < 3
;	  | f(n-1) + 2*f(n-2) + 3*f(n-3)
(define (fib n)
	(define (fib-iter count a b c)
		(cond ((= count n) a)
			  ((= count 0) (fib-iter 1 1 0 0))
			  ((= count 1) (fib-iter 2 2 1 0))
			  (else (fib-iter (+ count 1) (+ a (* 2 b) (* 3 c)) a b))))
	(fib-iter 0 1 0 0))


