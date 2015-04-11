#lang racket
(require test-engine/racket-tests)

(define x '((a / 1 0) c (d e f)))
(check-expect x (list (list 'a '/ 1 0)
                      'c (list 'd 'e 'f)))

(define y `(z (a ,x)))
(check-expect y (list 'z (list 'a x)))

(define z `(y ,(cdr (car x))))
(check-expect z (list 'y (list '/ 1 0)))

(test)