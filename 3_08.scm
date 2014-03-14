
(define f
    (let ((init 0))
        (lambda(x)
            (cond ((and (= x 0) (= init 0)) 
                    (begin (set! init 1) init))
                  ((= x 0) (+ init 1))
                  ((and (= x 1) (= init 0))
                    (begin (set! init 3) init))
                  ((= x 1) (+ init 1))))))

(+ (f 1) (f 0))

  
 (define (g y) 
   (define (f x) 
     (let ((z y)) 
       (set! y x) 
       z)) 
   f) 
 (define f (g 0)) 


