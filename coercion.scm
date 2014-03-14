
;   限制: 只能有两个参数. 要多参数自己扩展.
;   另外, 不能处理`跨层次转换`---除非你对每个层次都写个cast.     如t1->t2->t3, 这时候用这个过程就不行.
(define (apply-generic op . args)
    (let ((type-tags (map type-tag args)))
        (let ((proc (get op type-tags)))
            (if proc
                (proc args)
                (let ((t1 (car  type-tags))
                      (t2 (cadr type-tags))
                      (a1 (car  args))
                      (a2 (cadr args)))
                        (if (equ? t1 t2)        ;@ 处理同类型的情况
                            (error (error "No method for these typs" "APPLY-GENERIC" (list op type-tags)))
                            (let ((t1->t2 (get-coercion t1 t2))
                                  (t2->t1 (get-coercion t2 t1)))
                                    (cond (t1->t2 
                                            (apply-generic op (t1->t2 a1) a2))
                                          (t2->t1
                                            (apply-generic op a1 (t2->t1 a2)))
                                          (else (error "No method for these typs" "APPLY-GENERIC" (list op type-tags)))))))))))
