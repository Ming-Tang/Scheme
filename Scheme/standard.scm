;; Scheme standard library
;; This file is compiled into Scheme.dll as an embedded resource

(define nil '())
(define null '())
(define empty '())
(define true #t)
(define false #f)

(define first car)
(define rest cdr)

(define (foldl f x0 xs)
  (if (empty? xs) x0
    (foldl f (f x0 (car xs))
           (cdr xs))))

(define (foldr f x0 xs)
  (if (empty? xs) x0
    (f (car xs) (foldr f x0 (cdr xs)))))

(define (map f xs)
  (foldr (lambda (y ys) (cons (f y) ys))
         empty xs))

(define (caar x)
  (car (car x)))

(define (cadr x)
  (car (cdr x)))

(define (cdar x)
  (cdr (car x)))

(define (cddr x)
  (cdr (cdr x)))

