(define x 1)
(cons
 ((lambda (x y)
   (set! x (+ x y))
   x)
  10 5)
 x)
