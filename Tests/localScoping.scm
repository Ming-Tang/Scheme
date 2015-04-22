(define x 0)

(check-expect
  (let ([x 5]
        [y x])
    (list x y))
  (list 5 0))

(check-expect
  (let* ([y x]
         [x 10]
         [z x])
    (list x y z))
  (list 10 0 10))

(check-expect
  (letrec ([even (lambda (n)
                   (if (zero? n)
                     true
                     (odd (sub1 n))))]
           [odd (lambda (n)
                  (if (zero? n)
                    false
                    (even (sub1 n))))])
    (list (even 0) (odd 0) (even 1) (odd 1)
          (even 5) (odd 5) (even 10) (odd 10)))
  '(#t #f #f #t
    #f #t #t #f))

(check-expect
  (local [
    (define (even n)
      (if (zero? n)
        true
        (odd (sub1 n))))
    (define (odd n)
        (if (zero? n)
          false
          (even (sub1 n))))]
    (list (even 10) (odd 10)))
  '(#t #f))

