
(define (real-part z) (car z))
(define (imag-part z) (cdr z))
(define (magnitude z) 
    (sqrt (+ (square (real-part z)) (square (imag-part z)))))
(define (angle z) 
    (atan (imag-part z) (real-part z)))
(define (make-from-real-imag r i)
    (cons r i))
(define (make-from-mag-ang r a)
    (cons (* r (cos a)) (* r (sin a))))
(define (add-complex z1 z2)
    (make-from-real-imag (+ (real-part z1) (real-part z2))
                         (+ (imag-part z1) (imag-part z2))))
(define (sub-complex z1 z2)
    (make-from-real-imag (- (real-part z1) (real-part z2))
                         (- (imag-part z1) (imag-part z2))))
(define (mul-complex z1 z2)
    (make-from-mag-ang (* (magnitude z1) (magnitude z2))
                       (+ (angle z1) (angle z2))))
(define (div-complex z1 z2)
    (make-from-mag-ang (/ (magnitude z1) (magnitude z2))
                       (- (angle z1) (angle z2))))


;   为了实现最小允诺原则, 需要一种标记tag
(define (attach-tag type-tag contents)
    (cons (type-tag contents)))
(define (type-tag datum)
    (if (pair? datum)
        (car datum)
        (error "Bad tagged datum -- TYPE-TAG" datum)))
(define (content datum)
    (if (pair? datum)
        (car datum)
        (error "Bad tagged datum -- TYPE-TAG" datum)))
;   为了识别tag, 我们实现:
(define (rectangular? z)
    (eq? (type-tag z) 'rectangular))
(define (polar? z)
    (eq? (type-tag z) 'polar))
;   有了上面的过程, 两种复数的实现方式还是得避免重名, 所以在每种方式的op后添加上方式名字.
;<1> rectangular
(define (real-part-rectanglar z) (car z))
(define (imag-part-rectanglar z) (cdr z))
    ;我之前是想写个rec->mag&mag->rec, 但这个函数不知道该怎么归类, 所以就放着吧.
(define (magnitude-rectanglar z)
    (sqrt (+ (square (real-part-rectangular z))
             (square (imag-part-rectangular z)))))
(define (angle-ractanglar z)
    (atan (imag-part-rectangular z)
          (real-part-rectangular z)))
(define (make-from-real-imag-rectanglar x y)
    (attach-tag 'rectangular (cons x y)))
(define (make-from-mag-ang-rectangular m a)
    (attach-tag 'regtangular 
                (cons (* r (cos a)) (* r (sin a)))))
;<2> polar
(define (real-part-polar z)
    (* (magnitude-polar z) (cos (angle-polar))))
(define (imag-part-polar z)
    (* (magnitude-polar z) (sin (angle-polar))))
(define (magnitude-polar z) (car z))
(define (angle-polar z) (cdr z))
(define (make-from-real-imag-polar x y)
    (attach-tag 'polar
                (cons (sqrt (+ (square x) (square y))   )
                      (atan y x))))
(define (make-from-mag-ang-polar m a)
    (attach-tag 'polar
                (cons m a)))

;   有了上面的实现过程, 接下来就是generic procedure了
(define (real-part z)
    (cond ((ractangular? z)
            (real-part-rectangular z))
          ((polar? z)
            (real-part-polar z))
          (else (error "Unknown type -- REAL-PART" z))))

(define (imag-part z)
    (cond ((ractangular? z)
            (imag-part-rectangular z))
          ((polar? z)
            (imag-part-polar z))
          (else (error "Unknown type -- IMAG-PART" z))))
(define (magnitude z)
    (cond ((ractangular? z)
            (magnitude-rectangular z))
          ((polar? z)
            (magnitude-polar z))
          (else (error "Unknown type -- MAG-PART" z))))
(define (angle z) 
    (cond ((ractangular? z)
            (angle-rectangular z))
          ((polar? z)
            (angle-polar z))
          (else (error "Unknown type -- ANG-PART" z))))

;   generic operation
(define (add-complex z1 z2)     ;因为selectors是同用的, 所以可以随意选返回类型
    (make-from-real-imag (+ (real-part z1) (real-part z2))
                         (+ (imag-part z1) (imag-part z2))))





















































