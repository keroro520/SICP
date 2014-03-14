;; 习题2.2    习题2.3题目看不怎么明白, 不想写了

(define make-point cons)
(define x-point car)
(define y-point cdr)
(define (print-point p)
	(display "(")
	(display (x-point p))
	(display " , ")
	(display (y-point p))
	(display ")"))


(define (make-segment p1 p2)
	(cons p1 p2))
(define (start-segment l)
	(car l))
(define (end-segment   l)
	(cdr l))
(define (print-segment l)
	(newline)
	(display "[")
	(print-point (start-segment l))
	(display " . ")
	(print-point (end-segment   l))
	(display "]"))


;; 习题2.2之前的那些讲解

(define (make-rat n d) 
	(let ((g (gcd n d)))
		 (cons (quotient n g) (quotient d g))))

(define (number r) (car r))
(define (denom r)  (cdr r))

(define (add-rat x y)
	(make-rat (+ (* (number x) (denom y)) (* (denom x) (number y)))
			  (* (denom x) (denom y))))
(define (mul-rat x y)
	(make-rat (* (number x) (number y))
			  (* (denom x) (denom y))))

(define (equ-rat? x y)
	(and (= (number x) (number y)) (= (denom x) (denom y))))

(define (print-rat x)
	(newline)
	(display (number x))
	(display "/")
	(display (denom x)))
