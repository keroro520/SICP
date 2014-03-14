; 习题1.16: 

; 快速求幂
(define (square x) (* x x))

(define (exp-fast b n)
	(cond ((= 0 n) 1)
		  ((even? n) (square (exp-fast b (/ n 2))))
		  (else (* b (exp-fast b (- n 1))))))

; 变成迭代: 在计算过程中保存一个不变量(base)这一技术是思考迭代算法设计时一个强有力的方法
(define (exp-fast-iter base n)
		(define (exp-iter base ans n)
			(cond ((= 0 n) ans)
				  ((even? n) (exp-iter base (* ans ans) (/ n 2)))
				  (else (exp-iter base (* base ans) (- n 1)))))
		(exp-iter base 1 n))

