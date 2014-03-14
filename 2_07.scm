
;; 习题2.7
(define (make-interval a b) (cons a b))
(define (upper-bound seg) (max (car seg) (cdr seg)))
(define (lower-bound seg) (min (car seg) (cdr seg)))

(define (add-interval x y)
	(make-interval (+ (lower-bound x) (lower-bound y))
			       (+ (upper-bound x) (upper-bound y))))
(define (mul-interval x y)
	(let ((p1 (* (lower-bound x) (lower-bound y)))
		  (p2 (* (lower-bound x) (lower-bound y)))
		  (p3 (* (upper-bound x) (lower-bound y)))
		  (p4 (* (upper-bound x) (upper-bound y))))
	   	 (make-interval (max p1 p2 p3 p4)
		 			   (min p1 p2 p3 p4))))

(define (div-interval x y)
	(mul-interval x 
				  (make-interval (/ 1.0 (upper-bound y))
				  				 (/ 1.0 (lower-bound y)))))

(define a (make-interval 1 2))
(define b (make-interval 3 4))
(upper-bound a) (lower-bound b)
(add-interval a b)
(mul-interval a b)
(div-interval a b)