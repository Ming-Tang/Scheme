(define (f x)
  (define (f1 a b n)
    (if (= n 0)
        a
        (f1 b (+ a b) (- n 1))))
  (f1 1 1 x))

(define (table f n)
  (define (count i)
    (if (= i n)
        nil
        (cons (f i)
              (count (+ i 1)))))
  (count 0))

(table f 20)
