; execise 1_05

; fully expand and then reduce --> normal-order						完全展开而后规约

; evaluate the arguments and then apply --> applicative-order		先求值参数而后应用

; ATTENTION : "expand" and "evaluate" is different ! the result of 
; expanding is the extension of a procedure, but never evaluate .
; “扩展”只是形式地把子表达式扩展出来而已，如(define (p) (p)) 就只是扩展为(p)，而不是递归下去，因为它没有子表达式......貌似有点牵强 
; 而applicative-order会计算(p)的值，导致无限递归

; infinitely recuse with applicative-order 
; return 0 with normal-order

; Scheme use applicative-order

