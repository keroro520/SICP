
;   `Systems with Generic Operations`
;   借这个地方来写`通用算术系统`
;   之前的table技术实际上就是个`package`, 而这里就是想把所有的数的类型再打包, 再包一层即再多一层tag-type, 如('complex 'polar x)

; 习题2.77  略
; 习题2.78  
(define (attach-tag type-tags x)
    (if (number? x)
        x
        (cons type-tags x)))
(define (type-tag x)
    (cond ((number? x) 'scheme-number)
          ((pair? x) (car x))
          (else (error "" "" ""))))
(define (content x)
    (cond ((number? x) x)
          ((pair?   x) (cdr x))
          (else (error "" "" ""))))

; 习题2.79
;先在每个包里面定义个equal?
(define (equ? a b)
    (if (not (eq? (type-tag a) (type-tag b)))
        #f
        ((get (type-tag a) 'equ? a b)))
        ;也可以直接(generic-apply 'equ? a b)
