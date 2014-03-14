
;; 菲波那契约快速算法
;; [a   *  [1 0
;;	b]	    1 1]	==>  若a=f(n),b=f(n-1), 则这个式子相当于一次变换, 得出的结果是[f(n+1), f(n)]^T  .由此可以得出一个求菲波那契数列的方法, 即先求[[1 1]^T, [0 1]^T]的n次幂, 再与[a b]^T相乘
;; 题解: http://community.schemewiki.org/?sicp-ex-1.19

"fad"
(define (fib n)
	(define (fib-iter n a b p q)
		(cond ((= n 0) b)
			  ((even? n) 
			   (fib-iter (/ n 2) 
			   			 a 
						 b 
						 (+ (square p) (square q))
						 (+ (* 2 p q) (square q))))
			  (else (fib-iter (- n 1)	;因为这里漏了一个括号, 找了好久!
			  				 (+ (* q b) (* q a) (* p a))
							 (+ (* q a) (* p b))
							 p
							 q))))
	(fib-iter n 1 0 0 1))

(define (square x) (* x x))
