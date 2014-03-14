;; 习题2.37   很好的练习题!!


;; 写了习题之后才会感觉到, accumulator和accumulator-n的多么强大.


;; 写了一个不用map的dot-product, 比较感受一下两个版本的区别
;(define (dot-product v w)		;sum(vector * vector)
;	(define (help v w result)
;		(if (null? v)
;			result
;			(help (cdr v) (cdr w) (+ result (* (car v) (car w)))))))

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

(define (accumulator op init seqs)   
    (if (null? seqs)
        init
        (op (car seqs)
            (accumulator op init (cdr seqs)))))

;生成`每个子序列的尾元素`组成的序列
(define (accumulator-n op init seqs)
    (if (null? (car seqs))
        '()
        (cons (accumulator op init (enumerate-head-of-sequences seqs))
              (accumulator-n op init (enumerate-tail-of-sequences seqs)))))


(define (dot-product v w)
	(accumulator + 0 (map * v w)))

(define (matrix-*-vector m v)
	(map (lambda(w) (dot-product v w)) m))

(define (transpose mat)
	(accumulator-n 
		(lambda(x y) (cons x y))
		'()
		mat))

(define (matrix-*-matrix m n)
	(let ((cols (transpose n)))
		(map (lambda(v) (matrix-*-vector cols v)) m)))

;; Tesing
(define matrix (list (list 1 2 3 4) (list 5 6 7 8) (list 9 10 11 12))) 
(transpose matrix)
(matrix-*-matrix matrix (list (list 1 2) (list 1 2) (list 1 2) (list 1 2)))
