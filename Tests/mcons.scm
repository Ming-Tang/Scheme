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

