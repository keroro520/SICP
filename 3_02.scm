

;   习题3.2
(define (make-monitored f)
    (let ((calls 0))
        (lambda (x)
            (cond ((number? x)
                    (begin (set! calls (+ calls 1))
                           (f x)))
                  ((eq? 'how-many-calls? x)
                    calls)
                  ((eq? 'reset x) (set! calls 0))
                  (else (error "Unknown operatation -- MAKE-MONITORED" x))))))
