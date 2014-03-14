;; 嵌套映射		//TODO?? 什么意思?

;; 题意:对于1 <= j < i < n, 找出所有(i, j, i+j)使得(i+j)是素数
;; 一种很自然的组织方式是: 生成所有有序对(i,j), 过滤出和为素数的有序对, 对于过滤出来的每个有序对, 生成(i,j,i+j)


; 我理解的嵌套映射就是多重map
; 但是要注意! flatmap只是accumulate的简短版而已, 它是在里面间接地调用map, 所以是accumulate和map的结合版, 而不是"多重map".
; flatmap用的连接符是append. 因为enumerate-pair是以`(((1 2 3) (1 3 2))  (..) )`的形式, 所以append是为了让所有元素拼接起来...我之前都乱七八糟地想错了. 还不理解的话可是把enumerate-pair下的"flatmap"改为"map"看看, 当然编译不过, 不过可以执行(enumerate-pate 3)来看看结果.
; 所以说, flatmap没有它名字那么厉害, 别被骗了

; 即generate-triple为了作为flatmap的proc而写的. 当前版本是generate-triple单独出来的, 这应该是更为正确的理解, 而不是什么都要用到flatmap, flatmap应该是用在嵌套映射的情况.

; 我把它写成习题2.40了...-_-||  

(define (accumulate op initial seq)
	(if (null? seq)
		initial
		(op (car seq)
			(accumulate op initial (cdr seq)))))

(define (enumerate-interval first last)
	(if (> first last)
		'()
		(cons first (enumerate-interval (+ first 1) last))))

;(accumulate append 
;			'()
;			(map (lambda (i)	;两个map, 不一样的show
;				 	(map (lambda (j) (list i j))	;map可真强大
;						 (enumerate-interval 1 (- i 1))))
;				 (enumerate-interval 1 n)))

(define (flatmap proc seq)
	(accumulate append '() (map proc seq)))	
					;seq是(3元list)的pair, 所以用append

(define (enumerate-pair n)
	(flatmap (lambda(i)
				(map (lambda(j) (cons i j))
					 (enumerate-interval 1 (- i 1))))
			 (enumerate-interval 1 n)))

(define (prime-sum? p)
	(define (prime? num)
		(define (help i)
			(cond ((> i (sqrt num)) #t)
				  ((= 0 (modulo num i)) #f)
				  (else (help (+ i 1)))))
		(help 2))
	(prime? (+ (car p) (cdr p))))

(define (generate-triple seq)
	(map (lambda(x) (list (car x) (cdr x) (+ (car x) (cdr x))))
		 seq))
	
	;版本一的generate-triple是为了作为flatmap而写的, 不太直观, 而且实际上呢flatmap也不是为了这种情况而设计的 是我之前理解错了
	;(list (list (car p) (cdr p) (+ (car p) (cdr p))) ));(3元list)的pair
														  ;弄了好久

;; Tesing 
;(flatmap generate-triple		;版本一generate-trieple的test
;	(filter prime-sum? (enumerate-pair 5)))
(generate-triple (filter prime-sum? (enumerate-pair 5)))

