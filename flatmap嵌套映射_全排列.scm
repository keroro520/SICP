
;	与生成素数有序对`先生成range(1-n), 再扩展成pair-range`不同, 生成排列是一步做完成的, 而每个排列又需要是作为一个list元素存在在list里面的, 即list list, 所以最后的initial = (list '()), 而不是'()

;	又体会到了map的强大. map让我们从`面向整个list`摆脱出来, 到`面向单个元素`. 这题目如果用C来做, 没有map, 就得一个一个枚举..

(define (accumulate op initial seq)
	(if (null? seq)
		initial
		(op (car seq)
			(accumulate op initial (cdr seq)))))

(define (flatmap proc seq)
	(accumulate append '() (map proc seq)))

(define (remove x set)
	(filter (lambda(y) (not (= x y))) set))

(define (permutations set)
	(if (null? set)
		(list '())			;这里我可想不到, 是看书的. 注意!
		(flatmap (lambda (x)
					(map (lambda (return-pair) (cons x return-pair))
						 (permutations (remove x set))))
				 set)))

(permutations (list 1 2 3))
