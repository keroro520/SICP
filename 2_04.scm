;; 数据抽象导引

;; 只要满足: { 如果z是cons(x,y), 则(car z)为x, (cdr z)为y } 的三个过程, 就能成为实现序列的基础. Like below 

;; 实现序列的一种方式
(define (cons_2 x y)
	(lambda (m) 
		(cond ((= 0 m) x)
			  ((= 1 m) y)
			  (else (error "eval" "first argument should be 0 or 1")))))
(define (car_2 p) (p 0))
(define (cdr_2 p) (p 1))


;; 习题2.4   另一种实现序列的方式
(define (cons_3 x y)
	(lambda (m) (m x y)))
(define (car_3 p)
	(p (lambda (x y) x)))
(define (cdr_3 p)
	(p (lambda (x y) y)))
