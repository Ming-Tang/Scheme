(define x '((a / 1 0) c (d e f)))
(define y `(z (a ,x)))
(define (f x)
  (set! y `(y ,(cdr (car x)))))
(f x)
y
