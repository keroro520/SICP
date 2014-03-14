;	实例: 符号求导(Symbolic Differentiation)

;;	作者先假设这些东西已经存在, 然后就直接构造(deriv), 这种步骤倒是个好办法
; 
; (variable? e)
; Is e a variable?
; (same-variable? v1 v2) Are v1 and v2 the same variable?
; (sum? e) Is e a sum?
; (addend e) Addend of the sum e.
; (augend e) Augend of the sum e.
; (make-sum a1 a2) Construct the sum of a1 and a2.
; (product? e) Is e a product?
; (multiplier e) Multiplier of the product e
; (multiplicand e) Multiplicand of the product e.
; (make-product m1 m2) Construct the product of m1 and m2.
; 
(define (number= exp num)
	(and (number? exp) (= exp num)))
(define (variable? exp) (symbol? exp))
(define (same-variable? exp1 exp2) 
	(and (variable? exp1) (variable? exp2) (eq? exp1 exp2)))
(define (make-sum exp1 exp2)
	(cond ((and (number? exp1) (number? exp2)) (+ exp1 exp2))
		  ((number= exp1 0) exp2)
		  ((number= exp2 0) exp1)
	  	  (else (list '+ exp1 exp2))))
(define (make-product exp1 exp2)
	(cond ((and (number? exp1) (number? exp2)) (* exp1 exp2))
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
		  (else (error "unknown expressioni type" "here " exp))))


