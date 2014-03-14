
(define (same-key? key-1 key-2) (equal? key-1 key-2))

(define (assoc-new key table) ;@@
    (cond ((null? table) #f)
          ((same-key? key (caar table)) (car table))
          (else (assoc-new same-key? key (cdr table)))))

(define (make-table)
    (let ((local-table (list '*table*)))
        (define (lookup key-1 key-2)
            (let ((subtable (assoc-new key-1 (cdr local-table))))
                (if subtable
                    (let ((record (assoc-new key-2 (cdr subtable))))
                        (if record
                            (cdr record)
                            false))
                    false)))

        (define (insert! key-1 key-2 value)
            (let ((subtable (assoc-new key-1 (cdr local-table))))
                (if subtable
                    (let ((record (assoc-new key-2 (cdr subtable))))
                        (if record
                            (set-cdr! record value)
                            (set-cdr! subtable
                                      (cons (cons key-2 value)
                                            (cdr subtable)))))
                    (set-cdr! local-table
                              (cons (list key-1
                                          (cons key-2 value))
                                    (cdr local-table)))))
            'ok)

        (define (dispatch m)
            (cond ((eq? m 'lookup-proc) lookup)
                  ((eq? m 'insert-proc!) insert!)
                  (else (error "Unknown operation " " TABLE" m))))
    dispatch))

(define operation-table (make-table))
(define get (operation-table 'lookup-proc))
(define put (operation-table 'insert-proc!))
