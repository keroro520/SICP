

;; 习题2_27
;; 因为append的参数是两个list, 所以return value的时候记得加个(list)构造子
(define (deep-reverse l)
	(cond ((null? l) '())
		  ((not (pair? (car l))) 
		   (append (deep-reverse (cdr l)) (list (car l))))
		  (else (append (deep-reverse (cdr l))
		  				(list (deep-reverse (car l)))))))

(define l (list 1 2 (list 9 3 4) (list 5 6)))
(deep-reverse l)
