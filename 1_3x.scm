;; 习题1.31  1.32   1.33  
;; 高阶函数



(define (product a b f next-point)
    (if (> a b) 
        1
        (* (f a) (product (next-point a) b f next-point))))

;; testing
(define (factorial n)
    (define (inc x) (+ x 1))
    (define (f x) x)
    (product 1 n f inc))

;; 习题1.31
(define eval2-pi
    (define (next x) (+ x 2))
    (define (f x) (* (/ (- x 1) x) (/ (+ x 1) x)))
    (* 4 (product 3.0 100000 f next)))

;; 习题1.32     懒得验证
(define (accumulate combiner null-value term a next b)
    (if (> a b)
        null-value
        (combiner (term a) (accumulate combiner null-value term (next a) next b))))

;; 习题1.33
(define (filtered-accumulate combiner null-value term a next b filter)
    (cond ((> a b) null-value)
          ((filter a) (combiner (term a)
                                (filtered-accumulate combiner null-value term (next a) next b filter)))
          (else (filtered-accumulate combiner null-value term (next a) next b filter))))

(define (sum-integers a b)
    (if (= a b) 
        b
        (+ a (sum-integers (+ a 1) b))))

(define (sum-cubes a b)
    (define (cube x)
        (* x x x))
    (if (= a b) 
        (cube b)
        (+ (cube a) (sum-cubes (+ a 1) b))))

(define (sum a b term next)
    (if (> a b)
        ans
        (sum (next a) b term next (+ ans (term a)))))

