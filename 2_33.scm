
;;习题2.33
;; (1)
(define (accumulator op initial sequence)
	(if (null? sequence)
		initial
		(op (car sequence)
			(accumulator op initial (cdr sequence)))))

(define (map_2 p sequence)
	(accumulator 
		(lambda(x y) (cons (p x) y))
		'() sequence))

(map_2 (lambda(x) (* x x)) (list 1 2 3 5))

;; (2)
(define (append_2 seq1 seq2)
	(accumulator cons seq2 seq1))

(append_2 (list 1 2 3)  (list 4 5 6))

;; (3)
(define (length_2 sequence)
	(accumulator (lambda (_ y) (+ 1 y)) 0 sequence))

(length_2 (list 1 2 3 4))
