
;; 直接遍历树的版本
(define (scale-tree tree factor)
	(cond ((null? tree) '())
		  ((not (pair? tree)) (* tree factor))
		  (else (cons (scale-tree (car tree) factor)
		  			  (scale-tree (cdr tree) factor)))))

(scale-tree (list 1 (list 2 (list 3 4) 5) (list 6 7))
			10)

;; 用map 版本
(define (scale-tree-2 tree factor)		
	(map (lambda (sub-tree)
			(if (pair? sub-tree)
				(scale-tree-2 sub-tree factor)
				(* sub-tree factor)))
	     tree))
(scale-tree-2 (list 1 (list 2 (list 3 4) 5) (list 6 7))
			10)


;; 习题2.30
(define (square x) (* x x))
(define (square-tree tree)
	(cond ((null? tree) '())
		  ((pair? tree) (cons (square-tree (car tree))
		  					  (square-tree (cdr tree))))
		  (else (square tree))))

(square-tree (list 1 (list 2 (list 3 4) 5) (list 6 7)))

(define (square-tree-2 tree)
	(map (lambda (sub-tree)
		 	(if (pair? sub-tree)
			    (square-tree-2 sub-tree)
			    (square sub-tree)))
		 tree))

(square-tree-2 (list 1 (list 2 (list 3 4) 5) (list 6 7)))

;; 习题2.31
(define (tree-map f tree)
	(map (lambda (sub-tree)
			(if (pair? sub-tree)
				(tree-map f sub-tree)
				(f sub-tree)))
		 tree))

(define (square-tree-3 tree) (tree-map square tree))

(square-tree-3 (list 1 (list 2 (list 3 4) 5) (list 6 7)))
