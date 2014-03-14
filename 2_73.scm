;   习题2.73
; (a) number? same-variabal?是predicate, 不能作为dispatch

; 我觉得不用加tag, 因为tag只是因为标记复数的两种方式而已, 这里deriv只有一种表示方式.
(define (install-package-deriv)
    (define (number= exp num)
    	(and (number? exp) (= exp num)))
    (define (variable? exp) (symbol? exp))
    (define (same-variable? exp1 exp2) 
    	(and (variable? exp1) (variable? exp2) (eq? exp1 exp2)))
    (define (sum? exp)
    	(and (pair? exp) (eq? (car exp) '+)))
    (define (product? exp)
    	(and (pair? exp) (eq? (car exp) '*)))
    (define (addend exp) (cadr exp))
    (define (augend exp) (caddr exp))
    (define (multiplier exp) (cadr exp))
    (define (multiplicand exp) (caddr exp))

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
    (define (operator exp) (car exp))
    (define (operands exp) (cdr exp))
    (define (deriv exp var)
    	(cond ((number? exp) 0)
    		  ((variable? exp) (if (same-variable? var exp) 1 0))
              (else ((get 'deriv (operator exp)) (operands exp)     ;@
                                             var))))
    ;----@  (b)
    (put 'deriv '+ make-sum)        ;@
    (put 'deriv '* make-product)    ;@
    'done)
    ;@  (d) the only one thing to do is change the order of arguments in `put`
