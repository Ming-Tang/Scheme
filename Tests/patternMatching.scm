(check-expect
  (match 5
    [1 0]
    [x (add1 x)])
  6)
(check-expect
  (match 1
    [1 0]
    [x (add1 x)])
  0)
(check-expect
  (match 1
    [x (add1 x)]
    [1 0])
  2)

(check-error (match 1 [2 0]))

(check-expect
  (match "test"
    ["test" 1]
    [else 0])
  1)

(check-expect
  (match "hello"
    ["test" 1]
    [else 0])
  0)

(check-expect
  (match '(1 2 3)
    [() 0]
    [(x) x]
    [(x y) (+ x y)]
    [(x y z) (+ x (* y z))])
  (+ 1 (* 2 3)))

(check-expect
  (match '(1 2 3)
    [(a . b) `(dot ,a ,b)]
    [(a) `(single ,a)])
  '(dot 1 (2 3)))

(check-expect
  (match '(1 2)
    [(x y z ...) (list x y z)]
    [else '()])
  '(1 2 ()))

(check-expect
  (match '(1 2 3 4 5)
    [(x y z ...) (list x y z)])
  '(1 2 (3 4 5)))

(check-expect
  (match '(1 2 3 4 . 5)
    [(x y z ...) (list x y z)]
    [else '()])
  '())

;; Is association list?
(define (is-assoc? xs)
  (match xs
    [() #t]
    [((a b) c ...)
     (is-assoc? c)]
    [else #f]))

(check-expect (is-assoc? '()) #t)
(check-expect (is-assoc? '(1 2)) #f)
(check-expect (is-assoc? '((1 2) (3 4) (5 6))) #t)
(check-expect (is-assoc? '((1 2) (3 4) 0 (5 6))) #f)
