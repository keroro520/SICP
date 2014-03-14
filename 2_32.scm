
;; 习题2.32 : 生成集合的全部子集合.  算法简单而巧妙
(define (subsets s)
	(if (null? s)
		(list '())
		(let ((rest (subsets (cdr s))))
	      (append rest (map (lambda (x) (cons (car s) x)) rest)))))

(subsets (list 1 2 3 ))

