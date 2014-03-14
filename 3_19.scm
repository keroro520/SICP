; http://en.wikipedia.org/wiki/Cycle_detection

; Floyd cycle detection(又名Tortoise and hare)
; s表cycle的起始位置(注意, 不是list的起始)
; l表cycle的长
; 所以, Xs = Xs+l
; 
; 对于任意的i >= s和k >= 0 有Xi = Xi+kl
; 特殊地, 当i = kl >= s时, 有Xi = Xi+i (1)
; 所以, 该算法就是重复地检测list是否有满足形式(1),
; pa指向Xi,  每个step前进1个单位
; pb指向X2i, 每个step前进2个单位,  (其实想想就知道, 它这是为了满足i ~ 2*i的方法~)

(define (cycle? l)
    (define (safe-cdr l)
        (if (pair? l) (cdr l) '()))
    (define (iter front back)
        (cond ((not (and (pair? front) (pair? back))) #f)
              ((or (eq? front back)
                   (eq? front (safe-cdr back)))
                #t)
              (else (iter (safe-cdr front) (safe-cdr (safe-cdr back))))))
    (iter (safe-cdr l) (safe-cdr (safe-cdr l))))

