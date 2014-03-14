
; 习题3.14 
; 
; OA按题目执行完后, v = ('a), 这得小心. 因为global的x早就在第一遍就
; 被set-cdr!了

(define (mystery x)
        (define (loop x y)
                (if (null? x)
                    y
                    (let ((temp (cdr x)))
                         (set-cdr! x y)
                         (loop temp x))))
                         (loop x '()))
