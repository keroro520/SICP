
;   (put 'op-name '(arg1-type arg2-type ..) real-op-procedure)

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
(define (rectangular? z)
    (eq? (type-tag z) 'rectangular))
(define (polar? z)
    (eq? (type-tag z) 'polar))
;<1> rectangular
(define (install-package-rectangular)
    (define (real-part z) (car z))
    (define (imag-part z) (cdr z))
    (define (magnitude z)
        (sqrt (+ (square (real-part z))
                 (square (imag-part z)))))
    (define (angle z)
        (atan (imag-part z)
              (real-part z)))
    (define (make-from-real-imag x y)
        (attach-tag 'rectangular (cons x y)))
    (define (make-from-mag-ang m a)
        (attach-tag 'regtangular 
                    (cons (* r (cos a)) (* r (sin a)))))
    ;----
    (define (tag x) (attach-tag 'rectangular x))    ;@
    (put 'real-part '(rectangular) real-part)
    (put 'imag-part '(rectangular) imag-part)
    (put 'magnitude '(rectangular) magnitude)
    (put 'angle '(rectangular) angle)
    (put 'make-from-real-imag '(rectangular) 
        (lambda(x y) (tag (make-from-real-imag x y))))
    (put 'make-from-mag-ang   '(rectangular) 
        (lambda(m a) (tag (make-from-mag-ang m a))))
    'done)

;<2> polar
(define (install-package-polar)
    (define (real-part z)
        (* (magnitude z) (cos (angle z))))
    (define (imag-part z)
        (* (magnitude z) (sin (angle z))))
    (define (magnitude z) (car z))
    (define (angle z) (cdr z))
    (define (make-from-real-imag x y)
        (attach-tag 'polar
                    (cons (sqrt (+ (square x) (square y))   )
                          (atan y x))))
    (define (make-from-mag-ang m a)
        (attach-tag 'polar
                    (cons m a)))
    ;----
    (define (tag x) (attach-tag 'polar x))
    (put 'real-part '(polar) real-part)
    (put 'imag-part '(polar) imag-part)
    (put 'magnitude '(polar) magnitude)
    (put 'angle     '(polar) angle)
    (put 'make-from-real-imag '(polar) 
        (lambda(x y) (tag (make-from-real-imag x y))))
    (put 'make-from-mag-ang   '(polar) 
        (lambda(m a) (tag (make-from-mag-ang m a))))
    'done)


(define (apply-generic op . args)
    (let ((type-tags (map type-tag args)))
        (let ((proc (get op types-tags)))
            (if proc
                (apply proc (map contents args))
                (error "No method for these types   -- APPLY-GENERIC"
                       (list op type-tags))))))
;   generic operation
(define (add-complex z1 z2)     ;因为selectors是同用的, 所以可以随意选返回类型
    (make-from-real-imag (+ (real-part z1) (real-part z2))
                         (+ (imag-part z1) (imag-part z2))))

;利用apply-generic, 我们可以构造出:
(define (real-part z) (apply-generic 'real-part z))
(define (imag-part z) (apply-generic 'imag-part z))
(define (magnitude z) (apply-generic 'magnitude z))
(define (angle     z) (apply-generic 'angle      ))

;下面这些构造函数就得自己构造了, 无法借助apply-generic
(define (make-complex-from-real-imag x y)
    ((get 'make-from-real-imag 'rectangular) x y))
(define (make-complex-from-mag-ang r a)
    ((get 'make-from-mag-ang 'polar) r a))



















































