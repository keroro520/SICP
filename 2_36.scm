
;; 习题2.36
;; 跟上题一样, 就是看你会不会构造初始列表


;生成`每个子序列的头元素`组成的序列
(define (enumerate-head-of-sequences sequences)
	(if (null? sequences)
		'()
		(cons (car (car sequences))
			  (enumerate-head-of-sequences (cdr sequences)))))

(define (enumerate-tail-of-sequences sequences)
	(if (null? sequences)
		'()
		(cons (cdr (car sequences))
			  (enumerate-tail-of-sequences (cdr sequences)))))

(define (accumulate op init seqs)	
	(if (null? seqs)
		init
		(op (car seqs)
			(accumulate op init (cdr seqs)))))

;生成`每个子序列的尾元素`组成的序列
(define (accumulate-n op init seqs)
	(if (null? (car seqs))
		'()
		(cons (accumulate op init (enumerate-head-of-sequences seqs))
			  (accumulate-n op init (enumerate-tail-of-sequences seqs)))))

(define seqs (list (list 1 2 3) (list 4 5 6) (list 7 8 9)))
(accumulate-n + 0 seqs)
