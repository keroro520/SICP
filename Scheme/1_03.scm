
(define (Min x y)
	(if (< x y) x y))
(define (Sum x y)
	(+ x y))


(define (sum-of-the-max-2-numbers x y z)
	(- (+ x y z) (Min x (Min y z))))
