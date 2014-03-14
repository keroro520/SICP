
;; P73 的count-leaves  一开始我是用(list?), 但是(list?)遇到单单一个pair会false, 于是结果就跟内置length一样. 后来看书上用(pair?), 换了之后就真的是能算出叶子节点数了
(define (count-leaves l result)
	(cond ((null? l) result)
		  ((not (pair? (car l))) (count-leaves (cdr l) (+ result 1)))
		  (else (count-leaves (cdr l)
		  					  (count-leaves (car l) result)))))

(define l (list (list 1 2) (list 2 3) 5 ))
(count-leaves l 0)


;; 习题2.24
;; 翻前面的内容, 弄懂(list)的本质就好理解了.
;; 另外, 对于[  | -]->序列(分支)   !!!
;			  |
;			  v
;			  元素(子树)
