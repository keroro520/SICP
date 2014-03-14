
;;	Huffman tree   哈夫曼树

;(define (build-tree l)		;建树
;(define (decode bits tree)	;解码: 二进制序列 ==> 字母序列			反之没有写, 因为过程是一模一样的, 只不过是体力活而已

(define (make-leaf symbol weight) (list 'leaf symbol weight))
(define (symbol-leaf object) (cadr object))
(define (weight-leaf object) (caddr object))
(define (leaf? object) (eq? (car object) 'leaf))
(define (symbol object)
	(if (leaf? object) (list (symbol-leaf object)) (caddr object)))
(define (weight object)
	(if (leaf? object) (weight-leaf object) (cadddr object)))
(define (left-branch tree) (car tree))
(define (right-branch tree) (cadr tree))

(define (make-tree left right)
	(list left
		  right
		  (append (symbol left)		;这里用append的就需要(symbol)里面是返回leaf的symbol时有加`(list` (symbol-leaf obj)) 
		  		  (symbol right))
		  (+ (weight left) (weight right))))

(define (sort-list-for-tree l)	;在建树之前先把l按权重排列一下, 这里的l是原始l, 即(list (list 'a 1) (list 'b 2))的形式
	(map (lambda(x) (make-leaf (car x) (cadr x)))	;我用map比书上的make-leaf-set简单得多
		 (sort (lambda(x y) (< (cadr x) (cadr y))) l)))
(define (insert-list l x)		;把节点x(有可能是leaf也有可能是branch)插入到l里面, 这里的l是节点list, 即(list (list 'leaf 'a 1) (list '(a b) 2))的形式
	(cond ((null? l) (list x))
		  ((> (weight x) (weight (car l)))
		  	(cons (car l) (insert-list (cdr l) x)))
		  (else (cons x l))))

(define (list->tree l)
		(if (null? (cdr l))
			(list (car l))		;@@  这里很重要, 一开始我写"(car l)", 这样树建到最后只剩一个节点的时候就会变成(list '(a b c) 3)的形式, 因为如果(null? (cdr l))则l就是最后一个节点, 这样略丑. 于是改成了"(list (car l))", 变成(list (list '(a b c) 3))的格式.  NOTE: 所以如果要用到最后一棵树的话应该是(car l)喔
			(list->tree (insert-list (cddr l)
									 (make-tree (car l) (cadr l))))))

(define (build-tree l)
	(list->tree (sort-list-for-tree l)))

(define (decode bits tree)
	(define (choose-branch byte tree)
		(cond ((= 0 byte) (left-branch tree))
			  ((= 1 byte) (right-branch tree))
			  (else (error "Bad code" "0/1 expected" byte))))
	(define (decode1 bits cur-branch)
		(if (null? bits)
			'()
			(let ((next-branch (choose-branch (car bits) cur-branch)))
				(if (leaf? next-branch)
					(cons (symbol next-branch) (decode (cdr bits) tree))
					(decode1 (cdr bits) next-branch)))))
	(decode1 bits tree))

(define l (list (list 'a 2) (list 'b 5) (list 'c 3) (list 'd 4)))
l
(define tree (build-tree l))
(display tree)

(define (accumulate op init seg)
	(if (null? seg) init (op (car seg) (accumulate op init (cdr seg)))))
(define (show-decode l)
	(accumulate (lambda(x y) (cons (car x) y)) '() l))

(show-decode (decode (list 1 0 1 0 1 0 1 0 1  1 1 1 0 0 0 1) (car tree)))	;注意这里是(car tree), 因为我是用(list node)的形式来保存树的.







