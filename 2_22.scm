
;; 修改之后还不对, 解释的话回去重新看一遍pair是怎么构成list的那部分内容


;; 原题目程序 
(define (square-list-correct items)
	(define (iter things result)
		(newline)
		(display result)
		(if (null? things)
			result
			(iter (cdr things)
				  (cons (square (car things))
						result))))
	(iter items '()))

;; 正确程序
(define (square-list-correct items)
	(define (iter things result)
		(newline)
		(display result)
		(if (null? things)
			result
			(iter (cdr things)
				  (append result 
						  (list (square (car things)))))))
	(iter items '()))

(define l (list 1 2 3 4 ))
(square-list-correct l)
