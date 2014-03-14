;   数据抽象3种方式:
;   generic operations with explicit dispatch, data-directed style, and message-passing-style

;   generic operations with explicit dispatch : 通用过程是用一系列的cond跳转到相应op. 而且每个op还要注意重名
;   data-directed : 改用table技术, 有注册(put), 找相应函数(get), 这样, 不仅改进了通用过程的实现, 松耦合了generic-apply和函数的实现, 还消除了重名的担心
;   message-passing : oo技术, 直接一个对象就是一个dispatch, 不用管理operation, 因为所有op已经在对象里头了, 直接写op名即可.



;;  书上称之为`消息传递`...可我看就是oo嘛.
;;  [114]:这种组织方式的限制是只允许一个参数的generic procedure
(define (make-from-real-imag x y)
    (define (dispatch op)
        (cond ((eq? op 'real-part) x)
              ((eq? op 'imag-part) y)
              ((eq? op 'magnitude) (sqrt (+ (square x) (square y))))
              ((eq? op 'angle) (atan y x))
              (else (error "" "" ""))))
    dispatch)

(define (generic-apply op arg) (arg op))
