
(define (make-polynomial var terms)     ;包外定义
    ((get 'make 'polynomial) var terms))

(define (install-package-polynomial)
    ; representation of polynomial
    (define (adjoin-term term term-list)
        (if (=zero? (coeff term))
            (term-list
            (cons term term-list))))    ;? 这样会有序吗 ?
    (define (the-empty-termllist) '())
    (define (empty-terlist? term-list) (null? term-list))
    (define (first-term termlist) (car termlist))
    (define (rest-terms termlist) (cdr termlist))
    (define (make-term order coeff) (list order coeff))
    (define (order term) (car  term))
    (define (coeff term) (cadr term))

    ; manipulation
    (define (make-poly variable term-list) (cons variable term-list))
    (define (variable p) (car p))
    (define (term-list p) (cdr p))
    (define (same-variable? var1 var2) (eq? var1 var2))
    (define (tag p) (attach-tag 'polynomial p))
    (define (add-poly p1 p2)
        (if (same-variable? (variable p1) (variable p2))
            (make-poly (variable p1)
                       (add-terms (term-list p1)
                                  (term-list p2)))
            (error "Polys not in same var  -- ADD-POLY" (list p1 p2))))
    (define (mul-poly p1 p2)
        (if (same-variable? (variable p1) (variable p2))
            (make-poly (variable p1)
                       (mul-terms (term-list p1)
                                  (term-list p2)))
            (error "Polys not in same var  -- MUL-POLY" (list p1 p2))))
    
    ;term-list operations
    (define (add-terms L1 L2)
        (cond ((empty-termlist? L1) L2)
              ((empty-termlist? L2) L1)
              (else 
                (let ((t1 (first-term L1)) (t2 (first-term L2)))
                    (cond ((> (order t1) (order t2))
                            (adjoin-terms 
                                t1 (add-terms (rest-terms L1) L2)))
                          ((< (order t1) (order t2))
                            (adjoin-terms 
                                t2 (add-terms L1 (rest-terms L2))))
                          (else (adjoin-terms
                                    (make-term (order t1)
                                               (add (coeff t1) (coeff t2)))
                                    (add-terms (rest-terms L1) 
                                               (rest-terms L2)))))))))

    (define (mul-terms L1 L2)
        (if (empty-termlist? L1)
            (the-empty-termlist)
            (add-terms (mul-term-by-all-terms (first-term L1) L2)
                       (mul-term (rest-terms L1) L2))))
    (define (mul-term-by-all-terms t1 L)
        (if (empty-termlist? L)
            (the-empty-termlist)
            (let ((t2 (first-term L)))
                (adjoin-term
                    (make-term (+ (order t1) (order t2))
                               (mul (coeff t1) (coeff t2)))
                    (mul-term-by-all-terms t1 (rest-terms L))))))


    (put 'add '(polynomial polynomial) add-poly)
    (put 'mul '(polynomial polynomial) mul-poly)
    (put 'make 'polynomial
        (lambda (var terms) (tag (make-poly var terms))))
    'done)
























