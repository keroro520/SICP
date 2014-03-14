
; 区别: 用let的版本等于是多了一层scope, 如下图
;
; (1)global env :

; / \
;  |

; (2)`let` frame:

; / \
;  |

; (3)`lambda (amount)...` , 即frame that this procedure called 
