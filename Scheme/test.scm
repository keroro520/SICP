

(define (sqrt-Newton x) (sqrt- 1 x 1000))

(define (sqrt- guess x times)
	(if (= 0 times) 
		guess 
		(sqrt- (/ (+ (/ x guess) guess) 2) x (- times 1))))
			
(sqrt-Newton 9)
