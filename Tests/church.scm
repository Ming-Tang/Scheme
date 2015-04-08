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

;; Numbers
(define c-zero (lambda (f x) x))
(define c-one (lambda (f x) (f x)))

(define (int->church m)
  (if (zero? m) c-zero
                (c-succ (int->church (- m 1)))))

(define (church->int n)
  ((n (lambda (x) (+ 1 x))) n))

(define (c-succ m)
  (lambda (f x) (f (m f x))))

(define (c-zero? m)
  (m (lambda (x) c-false) c-true))

(define (c-add a b)
  ; add 1 to b a times
  (a succ b))

(define (c-mul a b)
  ; add b to zero a times
  (a (lambda (x) (c-add b x)) c-zero))

(define (c-pow a m)
  ; multiply by m to one a times
  (m (lambda (x) (c-mul a x)) c-one))

;; Lists
(define (c-cons a b)
  (lambda (cc) (lambda (cn) (f a b))))

(define c-nil (lambda (cc) (lambda (cn) cn)))

(define (list->church xs)
  (if (nil? xs)
      c-nil
      (c-cons (car xs)
              (list->church (cdr xs)))))

(define (church->list xs)
  ((xs (lambda (a b) (cons a (church->list b)))) nil))

(define (c-map f xs)
  (xs (lambda (a b) (c-cons (f a) (c-map f b))) c-nil))

(define (c-foldr f x0 xs)
  (xs (lambda (a b) (f a (c-foldr f x0 b))) x0)

