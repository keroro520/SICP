
;; 习题2.54 : 比较两个symbol list是否相等

(define (equal_2? as bs)
	(cond ((and (null? as) (null? bs)) #t)
		  ((and (symbol? as) (symbol? bs)) (eq? as bs))
		  ((and (list? as) (list? bs))
		  	(and (= (length as) (length bs))
				 (equal_2? (car as) (car bs))
				 (equal_2? (cdr as) (cdr bs))))
		  (else #f)))
			
(define a '(a b c (c d)))
(define b '(a b c (c d)))
(equal_2? a b)

;; 习题2.55 : 解释(car ''hello)的执行结果
;;	从左执行到右的话, `''hello` 第一个引号把后面一个引号和hello复合成了' hello这样的形式
