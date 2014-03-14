;; 素数检查: 费马小定理:a^n mod n = a mod n
;;           Miller-Rabin:a^(n-1) mod n = 1 mod n

(define (mr-prime? n times) ;Miller-Rabin素数检查
    (define (mr-test n)
        (= 1 (expmod (+ (random (- n 1)) 1) (- n 1) n)))
    (cond ((= 0 times) #t)
          ((mr-test n) (mr-prime? n (- times 1)))
          (else #f)))

(define (fast-prime? n times)       ;费马小定理
    (define (fermat-test n)
        (define (try-it a)
            (= (expmod a n n) a))
        (try-it (+ 1 (random (- n 1)))))
    (cond ((= times 0) #t)
          ((fermat-test n) (fast-prime? n (- times 1)))
          (else #f)))


(define (expmod base exp m)          ;一个数的幂对另一数取模(base^e) mod m
	(cond ((= exp 0) 1)
		  ((even? exp) 
		   (remainder (square (expmod base (/ exp 2) m)) 
		    		  m))
		  (else 
		   (remainder (* base (expmod base (- exp 1) m)) m))))

(define (square x) (* x x))

(define (exp-fast base n)
	(define (exp-iter count ans)
		(cond ((= 0 count) ans)
			  ((even? count)  (exp-iter (/ count 2) (square ans)))
			  (else (exp-iter (- count 1) (* ans base)))))
	(exp-iter n 1))

