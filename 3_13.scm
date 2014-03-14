
; 习题3.13

; 尝试的结果就是, 它确实跟C的pointer一样, 会围成一个环.
; 直接在REPL里会返回指针形式, 但display一下就infinite recussion了
;(define (last-pair x)
;    (if (null? x)
;        x
;        (last-pair (cdr x))))

(define (make-cycle x)
    (set-cdr! (last-pair x) x)
    x)

(define l (list 1 2))
(display (make-cycle l))
