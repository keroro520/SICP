

;;	无序single set
(define (element-of-set? x set)
	(cond ((or (null? set) (< x (car set))) #f)
		  ((= x (car set)) #t)
		  (else (element-of-set? x (cdr set)))))

(define (join-into-set x set)
	(cond ((null? set) (list x))
		  ((< x (car set)) (cons x set))
		  (else (cons (car set) (join-into-set x (cdr set))))))

(define (join-set x set)
	(if (element-of-set? x set)
		set
		(join-into-set x set)))

(define (list->set l)
	(if (null? l)
		'()
		(join-set (car l) (list->set (cdr l)))))

(define (union-set set1 set2)
	(cond ((null? set1) set2)
		  ((null? set2) set1)
		  ((< (car set1) (car set2)) 
		  	(cons (car set1) (union-set (cdr set1) set2)))
		  ((< (car set2) (car set1)) 
		  	(cons (car set2) (union-set set1 (cdr set2))))
		  (else (cons (car set1) (union-set (cdr set1) (cdr set2))))))

(define (intersection-set set1 set2)
	(if (or (null? set1) (null? set2)) 
		'()
		(let ((x1 (car set1)) (x2 (car set2)))
		  	(cond ((< x1 x2) (intersection-set (cdr set1) set2))
				  ((< x2 x1) (intersection-set set1 (cdr set2)))
				  (else (cons x1 (intersection-set (cdr set1) 
				  								   (cdr set2))))))))


(define s1 (list->set (list 1 7  329 3 91 3 0931  389)))
(define s2 (list->set (list 2 3 5 10 500 7 91 91 91)))
(union-set s1 s2)
(intersection-set s1 s2)
