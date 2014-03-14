; 千万要注意(define rand)  和(define (rand))的区别!!!


; 习题3.6

(define (rand-update x) (+ x 1))        ;TODO
(define random-init 5)
(define rand
    (let ((x random-init))
        (lambda (op)
            (if (eq? op 'reset)
                (lambda(new-x) (set! x new-x) x)
                (let ()
                    (set! x (rand-update x))
                    x)))))


(check 10)
(check 10)
((rand 'reset ) 3)
(check 10)
;
; (define (rand)... )版本, 未成功
;(define (rand)
;    (let ((x random-init))
;        (lambda (op)
;            (if (eq? op 'reset)
;                (lambda(new-x) (set! x new-x) x)
;                (let ()
;                    (set! x (rand-update x))
;                    x)))))
