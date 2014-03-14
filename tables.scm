


(define (lookup key-1 key-2 table)
    (let ((subtable (assoc key-1 (cdr table))))
        (if subtable
            (let ((recode (assoc key-2 (cdr subtable))))
                (if recode
                    (cdr recode)
                    #f))
            #f)))

(define (insert! key-1 key-2 value table)
    (let ((subtable (assoc key-1 (cdr table))))
        (if subtable
            (let ((recode (assoc key-2 (cdr table))))
                (if recode
                    (set-cdr! recode value)
                    (set-cdr! subtable 
                              (cons (cons key-2 value) (cdr subtable)))))
            (set-cdr! table 
                      (cons (list key-1 
                                 (cons key-2 value))
                            (cdr table)))))
    'ok)
                                    










(define (lookup key table)
    (let ((recode (assoc key (cdr table))))
        (if recode
            (cdr recode)
            #f)))
(define (assoc key l)
    (cond ((null? l) #f)
          ((equal? key (caar l)) (car l))
          (else (assoc key (cdr l)))))

(define (insert! key value table)
    (let ((recode (assoc key (cdr table))))
        (if recode
            (set-cdr! recode value)
            (set-cdr! table  
                      (cons (cons key value) (cdr table)))))
    'ok)

(define (make-table)
    (list '*table*))

