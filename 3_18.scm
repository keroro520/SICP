;@@ 为什么 append! 不能处理第一个参数为null的情况 ... ???!!!


;一开始用下面这个过程来实现给track拼接, but failded.
;@@ 因为这里的set! 不会改变外面的实参 !!!
;(define (set-cons! l e)
;        (set! l (append l (list e)))
;        (display l))

; @@ 那有什么办法可以以传参的方式改变external 的变量吗? 
; 就像C 是可以利用pointer.
(define (contains? l e)
    (cond ((null? l) #f)
          ((eq? (car l) e) #t)
          (else (contains? (cdr l) e))))

(define cycle? 
    (let ((track '()))
        (define (iter l)
            (cond ((null? l) #f)
                  ((contains? track (car l)) #t)
                  (else (begin (set! track (cons (car l) track))
                               (iter (cdr l))))))
        iter))


(define (make-cycle x)
    (set-cdr! (last-pair x) x)
    x)

(define l (make-cycle (cons 1 2)))
(cycle? (list 1 2 3))
(cycle? l)
