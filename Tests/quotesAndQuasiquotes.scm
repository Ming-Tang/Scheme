(define x '((a / 1 0) c (d e f)))
(check-expect x (list (list 'a '/ 1 0)
                      'c (list 'd 'e 'f)))

(define y `(z (a ,x)))
(check-expect y (list 'z (list 'a x)))

(define z `(y ,(cdr (car x))))
(check-expect z (list 'y (list '/ 1 0)))

(check-expect (quasiquote (1 2 (unquote (+ 1 2)) (unquote (- 5 1))))
              '(1 2 3 4))

(check-expect `(1 2 . ,(list 3)) '(1 2 3))
(check-expect `(1 2 ,@(list 3 4 5)) '(1 2 3 4 5))
(check-expect `(1 2 ,@(list 3 4 5) 6 7) '(1 2 3 4 5 6 7))
(check-expect `(,@(list 3 4 5) 6 . 7) '(3 4 5 6 . 7))

(check-expect `(1 2 `(,(+ 1 2) ,,(- 5 1))) '(1 2 `(,(+ 1 2) ,4)))

(check-expect `(1 2 ,@(list (+ 1 2) (- 5 1))) '(1 2 3 4))
