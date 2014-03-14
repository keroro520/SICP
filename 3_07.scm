
; 习题3.7    就是在3.3的基础上加上make-joint

; 思路即使对于每一个账户account维护一个password-list

(define (make-account passwd)
    (let ((balance 0)
          (password-list '()))  ;@
        (define (correct? pwd)  ;@
            (define (iter l)
                (cond ((null? l) #f)
                      ((eq? (car l) pwd) #t)
                      (else (iter (cdr l)))))
            (iter password-list))

        (define (withdraw amount)
            (if (>= balance amount)
                (begin (set! balance (- balance amount))
                       balance)
                "Insufficient funds"))
        (define (deposit amount)
            (set! balance (+ balance amount))
            balance)
        (define (joint new-password)    ;@
            (set! password-list (cons new-password password-list)))
        (define (dispatch2 op)
            (cond ((eq? op 'withdraw)   withdraw)
                  ((eq? op 'deposit)    deposit)
                  ((eq? op 'joint)      joint)
                  (else (error "Unknown request" "" op))))
        (define (dispatch pwd op)
            (if (correct? pwd)      ;@
                (dispatch2 op)
                (error "Incorrect --  password" pwd password-list)))
    
        (joint passwd)
        dispatch))

(define hello (make-account '123))
((hello '123 'deposit) 100)
((hello '123 'withdraw) 1)
((hello '123 'joint ) '456)
((hello '456 'withdraw) 3);
