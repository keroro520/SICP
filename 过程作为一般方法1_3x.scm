;; 过程作为一般性的方法
;; 习题1.37

;; 二分查找法  没有判断是否两点异号

(define (search f neg-point pos-point)
    (if (close-enough? neg-point pos-point)
        (/ (+ neg-point pos-point) 2)
        (let ((ava (/ (+ neg-point pos-point) 2)))
             (if (< 0 (f ava))
                 (search f neg-point ava)
                 (search f ava pos-point)))))

(define (close-enough? x y)
    (< (abs (- x y)) 0.001))


;; Testing  
(search (lambda (x) (- (* x x x) (* 2 x) 3))
    1 
    2)

;===========================

;; 找出函数的不动点     ;平方根
(define (fixed-point f first-guess)
    (define (close-enough? x y)
        (< (abs (- x y)) 0.00001 ))
    (let ((guess (f first-guess)))
         (if (close-enough? first-guess guess)
             guess
             (fixed-point f guess))))

(fixed-point cos 1.0)
(fixed-point (lambda (y) (/ (+ y (/ 2 y)) 2)) 1)    ; 牛顿法求平方根转为逼近法求不动点  用到SICP里称为"阻尼技术"的方法, 即取平均值法减少振荡

;; ==========================

;; 习题1.37 递归版  求黄金分割率
(define (infinite-continued-fraction n1 d1 n d times)
    (let ((n2 (n n1))
          (d2 (d d1)))
         (if (= 0 times)
             (/ n1 (+ d1 (/ n2 d2)))
             (/ n1 
                (+ d1 
                   (/ n2 
                      (+ d2 (infinite-continued-fraction n2 d2 n d (- times 1)))))))))


(define (fibonacci-number depth)
    (infinite-continued-fraction 1 1 (lambda (x) 1) (lambda (x) 1) depth))

(fibonacci-number 100)

;;  习题1.37 迭代版
(define (frac-iter n d times)
    (define (loop result times)
        (if (= 0 times)
            result
            (loop (/ (n times)
                     (+ (d times) result))
                  (- times 1))))
    (loop 0 times))

(frac-iter (lambda (x) 1) (lambda (x) 1) 100)

;; 习题1.38
(define eval-e
    (+ 2 (frac-iter (lambda (x) 1)
               (lambda (x) (if (= 0 (remainder (+ x 1) 3))
                               (* 2 (/ (+ x 1) 3))
                               1))
               100)))



