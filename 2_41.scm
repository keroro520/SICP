
;;	习题2.41 : 生成3元有序对(i,j,k), 1<=i<j<k<=n, i+j+k=s
;	之前书上的练习或习题的enumerate-pair都是生成(x . y)的形式, 但是这道题要3元有序组, 以之前的形式太丑了, 所以我写成了(i j k)的形式, 虽然过程麻烦了点.;; 看来对list/cons/append还不是很熟练, 都是尝试出来的.

;	下面注释与之前不同的点我用`@`标出
(define (accumulate op initial seq)
	(if (null? seq)
		initial
		(op (car seq)
			(accumulate op initial (cdr seq)))))

(define (flatmap proc seq)
	(accumulate append '() (map proc seq)))

(define (enumerate-interval first last)
	(if (> first last)
		'()
		(cons first (enumerate-interval (+ first 1) last))))
	
(define (enumerate-pair n)
	(flatmap (lambda (i)
				(map (lambda(j) (list i j))			; @
					 (enumerate-interval 1 (- i 1))))
			 (enumerate-interval 1 n)))

(define (enumerate-triple n)
	(flatmap (lambda (i)
				(map (lambda (j) (append i (list j))) 	;@
					 (enumerate-interval 1 (- (car (cdr i)) 1))))	;@
			 (enumerate-pair n)))

(define (triple-equal-sum n sum)
	(filter (lambda(x) (= sum (accumulate + 0 x))) (enumerate-triple n)))

;; Tesing
(triple-equal-sum 5 10)

