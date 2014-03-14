; 一个图形语言
;
;primitive operator:
;	beside	=>	left_p|right_p
;				first_p
;	below	=>	-------
;				second_p
;	flip-vert	=>	upside-down
;	flip-horiz	=>	left-to-right


;;	未抽象版本.   相当于`盖房子`
;	(define painter "-")		;primitive painter
;	(define wave painter)		;wave painter      starting from wave, creating a complex figure
;	(define wave2 (beside wave (flip-vert wave)))
;	(define wave4 (below wave2 wave2))

(define (flip-pairs painter)
	(let ((painter2 (flip-horiz painter)))
		(below painter2 painter2)))
(define wave4 (filp-pairs wave))

(define (right-split painter n)
	(if (= n 0)
		painter
		(let ((smaller (right-split painter (- n 1))))
			(beside painter (below smaller smaller)))))

(define (up-split painter n)
	(if (= 0 n)
		painter
		(let ((smaller (up-split painter (- n 1))))
			(below (beside smaller smaller) painter))))

