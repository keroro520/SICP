
;;	习题2.56

(define (number= exp num)
	(and (number? exp) (= exp num)))
(define (variable? exp) (symbol? exp))
(define (same-variable? exp1 exp2) 
	(and (variable? exp1) (variable? exp2) (eq? exp1 exp2)))
(define (make-sum exp1 exp2)
	(cond ((and (number? exp1) (number? exp2)) (+ exp1 exp2))	;@
		  ((number= exp1 0) exp2)
		  ((number= exp2 0) exp1)
	  	  (else (list '+ exp1 exp2))))
(define (make-product exp1 exp2)
	(cond ((and (number? exp1) (number? exp2)) (* exp1 exp2))	;@
		  ((or (number= exp1 0) (number= exp2 0)) '0)
		  ((number= exp1 1) exp2)
		  ((number= exp2 1) exp1)
		  (else (list '* exp1 exp2))))
(define (sum? exp)
	(and (pair? exp) (eq? (car exp) '+)))
(define (product? exp)
	(and (pair? exp) (eq? (car exp) '*)))
(define (addend exp) (cadr exp))
(define (augend exp) (caddr exp))
(define (multiplier exp) (cadr exp))
(define (multiplicand exp) (caddr exp))

;@
(define (exponentiation? exp)
	(and (pair? exp) (eq? (car exp) '^)))
(define (base exp) (cadr exp))
(define (exponent exp) (caddr exp))
(define (make-exponentiation b e) 
	(cond ((or (number= e 0) (number= b 1)) 1)
		  ((number= e 1) b)
		  (else (list '^ b e))))

(define (deriv exp var)
	(cond ((number? exp) 0)
		  ((variable? exp) (if (same-variable? var exp) 1 0))
		  ((sum? exp) (make-sum (deriv (addend exp) var)
		  						(deriv (augend exp) var)))
		  ((product? exp) 
		  	(make-sum (make-product (multiplier exp) 
									(deriv (multiplicand exp) var))
		  	   		  (make-product (multiplicand exp) 
					   				(deriv (multiplier exp) var))))
		  ((exponentiation? exp)		; @
		  	(make-product (exponent exp)
						  (make-product (make-exponentiation (base exp) 
						  									 (make-sum (exponent exp) -1))	;刚开始我直接(list '- xxx 1)
						  				(deriv (base exp) var))))
		  (else (error "unknown expressioni type" "here " exp))))


;; Tesing
(deriv '(^ u n) 'u)
(deriv '(+ (* 3 x) (* 2 x)) 'x)
