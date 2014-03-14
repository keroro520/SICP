
;message-orient 版本的queue

(define (make-queue)
    (let ((front-ptr '())
          (rear-ptr '()))
        
        (define (empty-queue?) (null? front-ptr ))
        (define (display-queue) (display front-ptr))
        (define (front-queue)
            (if (empty-queue?)
                (error "Exception" "FRONT called with an empty queue" "")
                (car front-ptr)))

        (define (set-front-ptr! item)
            (set! front-ptr item))    
        (define (set-rear-ptr!  item)
            (set! rear-ptr  item))

        (define (insert-queue! item)
            (let ((new-pair (cons item '())))
                (cond ((empty-queue?)
                        (set-front-ptr! new-pair)
                        (set-rear-ptr!  new-pair)
                        dispatch)
                      (else 
                        (set-cdr! rear-ptr  new-pair)
                        (set-rear-ptr! new-pair)
                        dispatch))))
        (define (delete-queue!)
            (cond ((empty-queue?)
                    (error "Exception " "DELETE called with an empty queue" ""))
                  (else 
                    (set-front-ptr! (cdr front-ptr))
                    dispatch)))
        
        (define (dispatch op)
            (cond ((eq? op 'insert) insert-queue!)
                  ((eq? op 'delete) delete-queue!)
                  ((eq? op 'display) display-queue)
                  ((eq? op 'front)  front-queue)
                  (else (error "Undefined operation" "MAKE-QUEUE" op))))
        

        dispatch))

(define q (make-queue))
((q 'insert) 1)
((q 'display))
((q 'insert) 2)
((q 'display))
((q 'insert) 3)
((q 'display))
((q 'delete))
((q 'display))
((q 'front))
