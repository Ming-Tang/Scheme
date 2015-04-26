(check-expect (promise? nil) #f)
(check-expect (promise? '(*promise*)) #f)
(check-expect (promise? (delay 1)) #t)

(check-expect (force (delay 'test)) 'test)
(check-expect (let* ([x 0]
                     [p (delay (begin
                                 (set! x (add1 x))
                                 x))])
                (list (force p) (force p)))
              (list 1 1))

