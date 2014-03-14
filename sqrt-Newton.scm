;牛顿法求平方根
(define (sqrt-Newton x)
	(sqrt-iter x 1))

(define (sqrt-iter x y)
	(define (good-enough? x y)
		(< (abs (- x (* y y))) 0.0001))
	(if (good-enough? x y)
		y
		(sqrt-iter x (improve x y))))


(define (improve x y)
	(/ (+ y (/ x y)) 2))
;;=================
;;牛顿法改进求平方根 : 时间上快得一笔呀~
;;改进是如果相邻两次猜测(guess)相差不超过0.001*guess就算成功, 不继续下去了
(define (sqrt-Newton-alt x)
	(sqrt-iter-alt x 1.0 2.0))
(define (sqrt-iter-alt x guess oldGuess)
	(define (good-enough? x guess oldGuess)
		(< (abs (- oldGuess guess)) 
		   (* guess 0.001)))
	(if (good-enough? x guess oldGuess)
		guess
		(sqrt-iter-alt x (improve x guess) guess)))

;;=================
(define (max1 x y z)
	(cond ((< x y ) (if (< y z) z y))
		  (else (if (< x z) z x))))

