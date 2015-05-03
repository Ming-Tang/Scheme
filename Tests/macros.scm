(define-macro (unless p? a b)
  `(if (,not ,p?) ,a ,b))

(define-macro (unless-1 p? a b)
  `(if (not ,p?) ,a ,b))

(define-macro (behave-as-begin! f)
  (let ([body (list 'quasiquote '(begin xs))])
    `(define-macro (,f . xs) ,body)))

(define-macro (swap! a b)
  (let ([temp (gensym)])
    `(let ([,temp ,b])
       (set! ,b ,a)
       (set! ,a ,temp))))

(define x 5)

(check-expect (unless (zero? x) (/ 10 x) 0) 2)
(check-expect
  (let ([not (lambda (x) x)])
    (unless (zero? x) (/ 10 x) 0)) 2)

(check-expect
  (let ([not (lambda (x) x)])
    (unless-1 (zero? x) (/ 10 x) 0)) 0)

(check-expect
  (let ([x 0]
        [y 0])
    (behave-as-begin! seq)
    (seq (set! x 1)
         (set! y 2)
         (list x y)))
  (list 1 2))

(check-expect
  (local [(define-macro (m1) 0)]
    (list (m1)
          (local [(define-macro (m1) 1)]
                 (m1))))
  (list 0 1))

(check-expect
  (let ([temp 123]
        [temp1 456])
    (swap! temp temp1)
    (list temp temp1))
  (list 456 123))

