;   3.1.1 Local State Variables

;   OO形式的account
(define (make-account balance)
    (define (withdraw amount)
        (if (>= balance amount)
            (begin (set! balance (- balance amount))
                   balance)
            "Insufficient funds"))
    (define (deposit amount)
        (set! balance (+ balance amount))
        balance)
    (define (dispatch op)
        (cond ((eq? op 'withdraw) withdraw)
              ((eq? op 'deposit ) deposit)
              (else (error "Unknown request  --MAKE-ACCOUNT" m))))

    dispatch)
;

(define balance 100)

(define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))

;把static变量通过let + lambda的方法实现对外不可见, 不错的想法.
(define new-withdraw        
    (let ((balance 200))
        (lambda (amount)
            (if (>= balance amount)
                (begin (set! balance (- balance amount))
                       balance)
                "没钱了"))))

(define (make-withdraw balance)
    (lambda (amount)
        (if (>= balance amount)
            (begin (set! balance (- balance amount))
                   balance)
            "没钱了亲")))

(new-withdraw 150)
(new-withdraw 60)
