
;; 习题2.28  


(define l (list  9 (list 1 2 3) (list 4 5) (list 6 7)))

;; 递归版本, 也是我比较喜欢的版本.
;; 虽然看起来像迭代, 但思想是递归思想, 而且else子句还有点巧妙.
(define (fringe tree result)
	(cond ((null? tree) result)
		  ((not (pair? tree)) (cons tree result))
		  (else (fringe (car tree)
		  				(fringe (cdr tree) result)))))

(fringe l '())

;; 借助append的版本
;;(define (fringe tree)
;;	(cond ((null? tree) '())
;;		  ((not (pair? (car tree)))
;;		  	(append (list (car tree)) (fringe (cdr tree))))
;;		  (else (append (fringe (car tree))
;;		  			    (fringe (cdr tree))))))
;;
;;(fringe l)

;; 失败的版本, 结果虽然也是叶子节点列表, 但顺序反过来了.
;;(define (fringe tree result)
;;	(cond ((null? tree) result)
;;		  ((not (pair? tree)) (cons tree result))
;;		  (else (fringe (cdr tree) (fringe (car tree) result)))))
;;
;;(fringe l '())


