(define (f1) 1)
(check-expect (f1) 1)
(check-error (f1 1))

(define (f2 x) x)
(check-error (f2))
(check-expect (f2 1) 1)
(check-error (f2 1 2))

(define (f3 x y z) (list x y z))
(check-expect (f3 1 2 3) (list 1 2 3))
(check-error (f3 1 2))
(check-error (f3 1 2 3 4))

(define (f4 a b . xs) `((,a ,b) . ,xs))
(check-error (f4))
(check-expect (f4 1 2) '((1 2)))
(check-expect (f4 1 2 3 4 5) '((1 2) 3 4 5))

(define (f5 . xs) xs)
(check-expect (f5) '())
(check-expect (f5 1 2 3) '(1 2 3))

(define l1 (lambda () 1))
(check-expect (l1) 1)
(check-error (l1 1))

(define l2 (lambda (x) x))
(check-error (l2))
(check-expect (l2 1) 1)
(check-error (l2 1 2))

(define l3 (lambda (x y z) (list x y z)))
(check-expect (l3 1 2 3) (list 1 2 3))
(check-error (l3 1 2))
(check-error (l3 1 2 3 4))

(define l4 (lambda (a b . xs) `((,a ,b) . ,xs)))
(check-error (l4))
(check-expect (l4 1 2) '((1 2)))
(check-expect (l4 1 2 3 4 5) '((1 2) 3 4 5))

(define l5 (lambda xs xs))

(check-expect (l5) '())
(check-expect (l5 1 2 3) '(1 2 3))
