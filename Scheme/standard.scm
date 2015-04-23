;; Scheme standard library
;; This file is compiled into Scheme.dll as an embedded resource

(define nil '())
(define null '())
(define empty '())
(define true #t)
(define false #f)

(define (void . xs) nil)

(define (add1 x) (+ 1 x))
(define (sub1 x) (- x 1))

(define (foldl f x0 xs)
  (if (empty? xs) x0
    (foldl f (f (car xs) x0)
           (cdr xs))))

(define (foldr f x0 xs)
  (if (empty? xs) x0
    (f (car xs) (foldr f x0 (cdr xs)))))

(define (map f xs)
  (foldr (lambda (y ys) (cons (f y) ys))
         empty xs))

(define (filter pred xs)
  (if (empty? xs) xs
    (if (pred (car xs))
      (cons (car xs) (filter pred (cdr xs)))
      (filter pred (cdr xs)))))

(define (reverse xs)
  (foldl cons empty xs))

(define (length xs)
  (foldr (lambda (a b) (+ 1 b)) 0 xs))

(define first car)
(define rest cdr)

(define (caar x)
  (car (car x)))

(define (cadr x)
  (car (cdr x)))

(define (cdar x)
  (cdr (car x)))

(define (cddr x)
  (cdr (cdr x)))

(define (caddr x)
  (car (cdr (cdr x))))

(define (cdddr x)
  (cdr (cdr (cdr x))))

(define (cadddr x)
  (car (cdr (cdr (cdr x)))))

(define (cddddr x)
  (cdr (cdr (cdr (cdr x)))))

;; Mutable pairs

(define (mcons x y)
  (define a x)
  (define b y)
  (define (get) (cons a b))
  (define (set-mcar! aa) (set! a aa))
  (define (set-mcdr! bb) (set! b bb))
  `(mcons ,get ,set-mcar! ,set-mcdr!))

(define (mcons? x)
  (and (cons? x)
       (equal? (car x) 'mcons)
       (cons? (cdr x))
       (lambda? (cadr x))
       (cons? (cddr x))
       (lambda? (caddr x))
       (cons? (cdddr x))
       (lambda? (cadddr x))
       (nil? (cddddr x))))

(define mpair? mcons?)

(define (mcar mc)
  (car ((cadr mc))))

(define (mcdr mc)
  (cdr ((cadr mc))))

(define (set-mcar! mc x)
  ((caddr mc) x))

(define (set-mcdr! mc x)
  ((cadddr mc) x))

(define (mcons->cons mc)
  (cons (mcar mc) (mcdr mc)))

(define (cons->mcons mc)
  (mcons (car mc) (cdr mc)))

(define (mlist->list ml)
  (if (mcons? ml)
    (cons (mcar ml) (mlist->list (mcdr ml)))
    ml))

(define (list->mlist l)
  (if (cons? l)
    (mcons (car l) (list->mlist (cdr l)))
    l))
