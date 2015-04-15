(check-expect (mcar (mcons 'a 'b)) 'a)
(check-expect (mcdr (mcons 'a 'b)) 'b)

(check-expect (mcons? (mcons 'a 'b)) true)
(check-expect (mcons? (cons 'a 'b)) false)
(check-expect (mcons? true) false)
(check-expect (mcons? cons) false)
(check-expect (mcons? nil) false)
(check-expect (mcons? 12) false)
(check-expect (mcons? 3.14) false)
(check-expect (mcons? "abc") false)
(check-expect (mcons? mcons?) false)

(check-expect
  (let [(mc1 (mcons 1 2))]
    (set-mcar! mc1 10)
    (mcar mc1))
  10)

(check-expect
  (let [(mc1 (mcons 1 2))]
    (set-mcdr! mc1 20)
    (mcdr mc1))
  20)

(check-expect (mcons->cons (mcons 1 2)) (cons 1 2))
(check-expect (mcar (cons->mcons (cons 1 2))) 1)
(check-expect (mcdr (cons->mcons (cons 1 2))) 2)

(check-expect (mlist->list (mcons 1 (mcons 2 (mcons 3 (mcons 4 nil)))))
              '(1 2 3 4))

(check-expect
  (let [(ml (list->mlist '(1 2 3)))]
    (list (mcar ml)
          (mcar (mcdr ml))
          (mcar (mcdr (mcdr ml)))
          (mcdr (mcdr (mcdr ml)))))
  (list 1 2 3 nil))

