;; 习题2.6  丘奇数
;; 参考: http://en.wikipedia.org/wiki/Church_encoding
;;	   : http://community.schemewiki.org/?sicp-ex-2.6


; zero接受一个f作为参数, 后面那个f的参数x可以不要太管, 加上(lambda (x) x)估计只是为了程序的完整性, 对对理解Church number没什么作用.
(define zero (lambda (f) (lambda (x) x)))	


; 可以这么理解, 因为n = \(n f), 即(f^n)(x)f的n阶函数. 然后n+1则可以表示为\f (f n), 即f^(n+1)
(define (add-one n)		
	(lambda (f) (lambda (x) (f ((n f) x)))))

(define one (lambda (f) (lambda (x) (f x))))
;; 也可以利用上面的zero, 写成(define one (lambda (f) (lambda (x) (f ((zero f) x))))

(define two (lambda (f) (lambda (x) (f (f x)))))

(define (add n m)
	(lambda (f)
		(lambda (x)
			((n f) ((m f) x)))))
