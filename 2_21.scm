
;; 习题2.21
(define (square x) (* x x))
(define (square-list items)
	(if (null? items)
		'()
		(cons (square (car items)) (square-list (cdr items)))))

(define (square-list-2 items)
	(map square items))

(define l (list 1 2 3 43 5 36))
(square-list l)
(square-list-2 l)
