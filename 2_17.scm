
;; 习题2.17 
(define (last-pair-2 l)
	(if (null? (cdr l))
		(list (car l))
		(last-pair-2 (cdr l))))

;; 习题2.18
(define (reverse-2 l)
	(if (null? l)
		'()
		(append (reverse-2 (cdr l)) (list (car l)))))
;; =========
(define (list-at l index)
	(if (= 0 index)
		(car l)
		(list-at (cdr l) (- index 1))))

(define (list-length l)
	(if (null? l)
		0
		(+ 1 (list-length (cdr l)))))


;; 下面是两种append, 结果不一样, 我不明白(cons l 9) 和(cons 9 l)的本质区别是什么

(define (list-append l1 l2)
	(if (null? l1)
		l2
		(cons (car l1) (list-append (cdr l1) l2))))

(define (list-append_2 l1 l2)
	(if (null? l2)
		l1
		(list-append_2 (cons l1 (car l2)) (cdr l2))))
