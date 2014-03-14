;   习题3.5
;   用蒙特卡罗积分求在矩形m内, 满足条件p的区域面积

(define (estimate-integal p x1 x2 y1 y2)
    (define lowx  (if (< x1 x2) x1 x2))
    (define highx (if (< x1 x2) x2 x1))
    (define lowy  (if (< y1 y2) y1 y2))
    (define highy (if (< y1 y2) y2 y1))

    (define trials 10000)
    (define (cesaro-test)
        (if (p (random-in-range lowx highx)
               (random-in-range lowy highy)) 
            1 
            0))
    (define (monte-carlo trials)
        (define (iter trials-remaining result)
            (cond ((= trials-remaining 0) result)
                  (else (iter (- trials-remaining 1) 
                        (+ result (cesaro-test))))))
        (iter trials 0))

    (* (/ (monte-carlo trials) trials) (abs (* (- x1 x2) (- y1 y2)))))


(define (random-in-range low high)
    (let ((range (- high low)))
        (+ low (random range))))
(define (square x) (* x x))
(estimate-integal 
    (lambda(x y)
        (< (+ (square (- x 5)) (square (- y 7))) (square 3)))
    2 8 4 10)
