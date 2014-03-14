
;; 习题2.35
;; 难点在序列的构造, 即accumulate的一开始那个要处理的序列, enumerate-leave

(define (accumulate op initial sequence)
	(if (null? sequence)
		initial
		(op (car sequence)
			(accumulate op initial (cdr sequence)))))
(define (enumerate-leave t)	;多少片叶子, 就产生多少个元素皆为1的list
	(cond ((null? t) '())
		  ((not (pair? (car t))) (cons 1 (enumerate-leave (cdr t))))
		  (else (append (enumerate-leave (car t))
			  		    (enumerate-leave (cdr t))))))

(define (count-leaves t)
	(accumulate + 0 (map (lambda(x) 1) (enumerate-leave t))))

(count-leaves (list 1 (list 2 3) (list 4 (list 5 6))))
