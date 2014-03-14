

(define (make-queue)
    (cons '() '()))
(define (empty-queue? q) (null? (front-ptr q)))

(define (front-ptr q) (car q))
(define (rear-ptr  q) (cdr q))

(define (front-queue q)
    (if (empty-queue? q)
        (error "Exception" "FRONT called with an empty queue" q)
        (car (front-ptr q))))

(define (rear-queue  q) (cdr q))

(define (set-front-ptr! queue item)
    (set-car! queue item))    
(define (set-rear-ptr!  queue item)
    (set-cdr! queue item))

(define (insert-queue! q item)
    (let ((new-pair (cons item '())))
        (cond ((empty-queue? q)
                (set-front-ptr! q new-pair)
                (set-rear-ptr!  q new-pair)
                q)
              (else 
                (set-cdr! (rear-ptr q) new-pair)
                (set-rear-ptr! q new-pair)
                q))))
(define (delete-queue! q)
    (cond ((empty-queue? q)
            (error "Exception " "DELETE called with an empty queue" q))
          (else 
            (set-front-ptr! q (cdr (front-ptr q)))
            q)))

(define q (make-queue))
(insert-queue! q 1)
(insert-queue! q 2)
(insert-queue! q 3)
(insert-queue! q 4)
(insert-queue! q 5)
(delete-queue! q)
(delete-queue! q)
(delete-queue! q)

