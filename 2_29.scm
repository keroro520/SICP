;; 习题2.29, 没写完, 因为不理解题目中"平衡"的定义.
;; 不过也算写完了吧, 因为只要弄懂了平衡的含义, 再修改了一下balance就可以了

(define (make-mobile left right) (list left right))
(define (make-branch length structure) (list length structure))

(define (left-branch  mobile) (car mobile))
(define (right-branch mobile) (car (cdr mobile)))

(define (branch-length branch) (car branch))
(define (branch-structure branch) (car (cdr branch)))

(define (is-leave? mobile)
	(and (integer? (branch-length mobile))
		 (integer? (branch-structure mobile))))

(define (is-mobile? mobile)
	(and (not (integer? (left-branch mobile)))
		 (not (integer? (right-branch mobile)))))

(define (total-weight mobile)
	(if (is-leave? mobile)
		(branch-structure mobile)
		(+ (total-weight (left-branch  mobile))
		   (total-weight (right-branch mobile)))))

(define (moment branch)
	(* (branch-length branch)
	   (total-weight  branch)))

(define (balance mobile)
	(cond ((is-leave? mobile) #t)
		  ((is-mobile? mobile)
		   (and (balance (left-branch mobile))
			    (balance (right-branch mobile))))
		  (else (= (moment (left-branch mobile))	;else is branch
			 	   (moment (right-branch mobile))))))


;; Tesing
(define l_1 (make-branch 10 10))
(define l_2 (make-branch 1 100))
(define l (make-branch l_1 l_2))	; test的时候写成(make-branch 1 l_1), 囧
(define r (make-branch 3 4))
(define m (make-mobile l r))
(is-leave? r)
(is-leave? l)
(total-weight r)
(total-weight l)
(total-weight m)
(balance r)
(balance l)
;(balance m)
(moment l)

(balance l_1)
(balance l_2)
