
(define (make-account passwd)
    (let ((balance 0))
        (define (withdraw amount)
            (if (>= balance amount)
                (begin (set! balance (- balance amount))
                       balance)
                "Insufficient funds"))
        (define (deposit amount)
            (set! balance (+ balance amount))
            balance)
        (define (dispatch2 op)
            (cond ((eq? op 'withdraw) withdraw)
                  ((eq? op 'deposit) deposit)
                  (else (error "Unknown request") op)))
        (define (dispatch pwd op)
            (if (eq? pwd passwd)
                (dispatch2 op)
                (error "Incorrect --  password" pwd pwd)))
    
        dispatch))

(define hello (make-account '123))

((hello '123 'deposit) 100)
((hello '123 'withdraw) 100)
       
((hello '123 'fa) 1)
