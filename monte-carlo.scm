
(define (rand-update x) (+ x 1))        ;TODO
(define random-init 5)
(define rand
    (let ((x random-init))
        (lambda ()
            (set! x (rand-update x))
            x)))

(define (estimate-pi trials)
    (sqrt (/ 6 (monte-carlo trials cesaro-test))))

(define (cesaro-test)
    (if (= (gcd (rand) (rand)) 1) 1 0))

(define (monte-carlo trials experiment)
    (define (iter trials-remaining result)
        (cond ((= trials-remaining 0) result)
              (else (iter (- trials-remaining 1) (+ result (experiment))))))
    (iter trials 0))

(estimate-pi 10000)
