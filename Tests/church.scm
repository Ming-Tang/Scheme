;; Church encoding

;; Booleans
(define c-true (lambda (a b) a))
(define c-false (lambda (a b) b))

(define (bool->church b)
  (if b c-true c-false))

(define (church->bool c)
  (c true false))

(define (c-and a b)
  (a b c-false))

(define (c-or a b)
  (a c-true b))

(define (c-not b)
  (b c-false c-true))

(define (c-if b f g)
  ((b f g)))

(check-expect (c-true 1 2) 1)
(check-expect (c-false 1 2) 2)

(check-expect ((bool->church true) 1 2) 1)
(check-expect ((bool->church false) 1 2) 2)

(check-expect (church->bool c-true) true)
(check-expect (church->bool c-false) false)

(check-expect (church->bool (c-and c-false c-false)) false)
(check-expect (church->bool (c-and c-true c-false)) false)
(check-expect (church->bool (c-and c-false c-true)) false)
(check-expect (church->bool (c-and c-true c-true)) true)

(check-expect (church->bool (c-or c-false c-false)) false)
(check-expect (church->bool (c-or c-true c-false)) true)
(check-expect (church->bool (c-or c-false c-true)) true)
(check-expect (church->bool (c-or c-true c-true)) true)

(check-expect (church->bool (c-not c-false)) true)
(check-expect (church->bool (c-not c-true)) false)

(check-expect (c-if c-true
                    (lambda () "success")
                    (lambda () (error "failure"))) "success")
(check-expect (c-if c-false
                    (lambda () (error "failure"))
                    (lambda () "success")) "success")

;; Numbers
(define c-zero (lambda (f x) x))
(define c-one (lambda (f x) (f x)))

(define (int->church m)
  (if (zero? m) c-zero
                (c-succ (int->church (- m 1)))))

(define (church->int n)
  (n (lambda (x) (+ 1 x)) 0))

(define (c-succ m)
  (lambda (f x) (f (m f x))))

(define (c-pred m)
  ; create a list of length m, get its cdr, and get its length
  ((c-repeat c-succ m) (lambda (a b) (c-length b)) c-zero))

(define (c-zero? m)
  (m (lambda (x) c-false) c-true))

(define (c-add a b)
  ; add 1 to b a times
  (a c-succ b))

(define (c-sub a b)
  ; subtract 1 from a b times
  (b c-pred a))

(define (c-mul a b)
  ; add b to zero a times
  (a (lambda (x) (c-add b x)) c-zero))

(define (c-pow a m)
  ; multiply by m to one a times
  (m (lambda (x) (c-mul a x)) c-one))

(define (c-fac n)
  (c-if (c-zero? n)
        (lambda () c-one)
        (lambda () (c-mul n (c-fac (c-pred n))))))

(define (c-fib n)
  (c-if (c-zero? n)
        (lambda () c-one)
        (lambda ()
          (c-if (c-zero? (c-pred n))
                (lambda () c-one)
                (lambda () (c-add (c-fib (c-pred n))
                                  (c-fib (c-pred (c-pred n)))))))))

(check-expect (c-zero c-zero? "success") "success")
(check-expect (c-one (lambda (x) "success") "fail") "success")

(check-expect (church->bool (c-zero? (int->church 0))) true)
(check-expect (church->bool (c-zero? (int->church 10))) false)

(check-expect (church->int c-zero) 0)
(check-expect (church->int c-one) 1)
(check-expect (church->int (int->church 5)) 5)

(check-expect ((c-succ (int->church 5)) (lambda (x) (+ 10 x)) 0) 60)
(check-expect ((c-pred (int->church 5)) (lambda (x) (+ 10 x)) 0) 40)
(check-expect ((c-pred c-zero) (lambda (x) (+ 10 x)) 0) 0)

(check-expect (church->int (c-add (int->church 0) (int->church 0))) 0)
(check-expect (church->int (c-add (int->church 5) (int->church 0))) 5)
(check-expect (church->int (c-add (int->church 0) (int->church 5))) 5)
(check-expect (church->int (c-add (int->church 5) (int->church 3))) 8)
(check-expect (church->int (c-add (int->church 3) (int->church 5))) 8)

(check-expect (church->int (c-sub (int->church 5) (int->church 0))) 5)
(check-expect (church->int (c-sub (int->church 0) (int->church 5))) 0)
(check-expect (church->int (c-sub (int->church 5) (int->church 3))) 2)
(check-expect (church->int (c-sub (int->church 3) (int->church 5))) 0)

(check-expect (church->int (c-mul (int->church 0) (int->church 0))) 0)
(check-expect (church->int (c-mul (int->church 5) (int->church 1))) 5)
(check-expect (church->int (c-mul (int->church 1) (int->church 5))) 5)
(check-expect (church->int (c-mul (int->church 5) (int->church 3))) 15)
(check-expect (church->int (c-mul (int->church 3) (int->church 5))) 15)

(check-expect (church->int (c-pow (int->church 0) (int->church 0))) 1)
(check-expect (church->int (c-pow (int->church 1) (int->church 0))) 1)
(check-expect (church->int (c-pow (int->church 2) (int->church 1))) 2)
(check-expect (church->int (c-pow (int->church 3) (int->church 0))) 1)
(check-expect (church->int (c-pow (int->church 0) (int->church 3))) 0)
(check-expect (church->int (c-pow (int->church 5) (int->church 3))) 125)

(check-expect (church->int (c-fac (int->church 0))) 1)
(check-expect (church->int (c-fac (int->church 1))) 1)
(check-expect (church->int (c-fac (int->church 2))) 2)
(check-expect (church->int (c-fac (int->church 3))) 6)
(check-expect (church->int (c-fac (int->church 4))) 24)
(check-expect (church->int (c-fac (int->church 5))) 120)
(check-expect (church->int (c-fac (int->church 6))) 720)

(check-expect (church->int (c-fib (int->church 0))) 1)
(check-expect (church->int (c-fib (int->church 1))) 1)
(check-expect (church->int (c-fib (int->church 2))) 2)
(check-expect (church->int (c-fib (int->church 3))) 3)
(check-expect (church->int (c-fib (int->church 4))) 5)
(check-expect (church->int (c-fib (int->church 5))) 8)
(check-expect (church->int (c-fib (int->church 6))) 13)
(check-expect (church->int (c-fib (int->church 7))) 21)

;; Lists
(define (c-cons a b)
  (lambda (cc cn) (cc a b)))

(define c-nil (lambda (cc cn) cn))

(define (list->church xs)
  (if (nil? xs)
      c-nil
      (c-cons (car xs)
              (list->church (cdr xs)))))

(define (church->list xs)
  (xs (lambda (a b) (cons a (church->list b))) empty))

(define (c-map f xs)
  (xs (lambda (a b) (c-cons (f a) (c-map f b))) c-nil))

(define (c-foldr f x0 xs)
  (xs (lambda (a b) (f a (c-foldr f x0 b))) x0))

(define (c-length xs)
  (c-foldr (lambda (a b) (c-succ b)) c-zero xs))

(define (c-repeat x n)
  (n (lambda (l) (c-cons x l)) c-nil))

(check-expect (c-nil list nil) nil)
(check-expect ((c-cons 1 2) list nil) (list 1 2))

(check-expect (church->int
               (c-foldr c-add c-zero
                        (list->church (map int->church nil))))
              0)
(check-expect (church->int
               (c-foldr c-add c-zero
                        (list->church (map int->church (list 1 2 3 4 5)))))
              (+ 1 2 3 4 5))

(check-expect (church->list (c-map (lambda (x) (* x 10))
                                   (list->church (list 1 2 3 4 5))))
              (list 10 20 30 40 50))

(check-expect (church->list (c-repeat 0 c-zero))
              nil)
(check-expect (church->list (c-repeat 0 (int->church 5)))
              (list 0 0 0 0 0))

(check-expect (church->int (c-length (list->church (list 1 2 3 4 5)))) 5)
