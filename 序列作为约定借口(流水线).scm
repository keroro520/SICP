

(define (square x) (* x x))
(define (sum-odd-squares tree)
	(cond ((null? tree) 0)
		  ((not (pair? tree))
		   (if (odd? tree) (square tree) 0))
		  (else (+ (sum-odd-squares (car tree))
		  		   (sum-odd-squares (cdr tree))))))
(sum-odd-squares (list 1 2 (list 3 4) (list 5 6) 7))

(define (sum-tree filter f tree)
	(map f (filter tree)))

; 内置有filter了...
;(define (filter predicate sequence)
;	(cond ((null? sequence) '())
;		  ((predicate (car sequence))
;		   (cons (car sequence) (filter predicate (cdr sequence))))
;		  (else (filter predicate (cdr sequence)))))

(define (accumulator op initial sequence)
	(if (null? sequence)
		initial
		(op (car sequence)
			(accumulator op initial (cdr sequence)))))

(define (enumerate-interval bottom top)
	(if (> bottom top)
		'()
		(cons bottom (enumerate-interval (+ bottom 1) top))))
(define (enumerate-tree tree)
	(cond ((null? tree) '())
		  ((not (pair? tree)) (list tree))
		  (else (append (enumerate-tree (car tree))
		  				(enumerate-tree (cdr tree))))))

(define (fib n)
	(cond ((= 1 n) 1)
		  ((= 2 n) 2)
		  (else (+ (fib (- n 1)) (fib (- n 2))))))

(define (list-of-odd-squares tree)
	(accumulator 
		cons
		'()
		(map square								;精彩部分在后面3句
			 (filter odd?						;而不是accumulator本身
			 		 (enumerate-tree tree)))))	;后面习题才体现accumulator

(define tree (list 1 (list 2 3) (list 4 (list 5 6) 7 ) ))
(list-of-odd-squares tree)

(define (list-of-even-fib bottom top)
	(accumulator
		cons
		'()
		(map fib 
			 (filter even? 
			 		 (enumerate-interval bottom top)))))

(list-of-even-fib 1 10)














