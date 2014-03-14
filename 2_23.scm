;; 不知道怎么在if里面执行两条语句, //TODO

(define (my-for-each f iterms)
	(if (null? iterms)
		(newline)
		(and (f (car iterms))		;MARK
			 (my-for-each f (cdr iterms)))))

(my-for-each (lambda (x) (newline) (display x))
			 (list 1 2 3 4 ))
