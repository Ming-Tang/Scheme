(check-expect (+ 0 0) 0)
(check-expect (+ 15 3) 18)
(check-expect (+ 15 -20) -5)
(check-within (+ 15.0 -20) -5.0 1e-5)
(check-within (+ 1 2.0) 3.0 1e-5)
(check-within (+ 8.0 1000.0) 1008.0 1e-5)
(check-within (+ 10 1.0 10.0 0) 21.0 1e-5)

(check-expect (- 0 0) 0)
(check-expect (- 10 3) 7)
(check-within (- 10 3.0) 7.0 1e-5)
(check-within (- 10.0 3) 7.0 1e-5)
(check-within (- 5.0 2.0) 3.0 1e-5)

(check-expect (* 0 0) 0)
(check-expect (* 1 5) 5)
(check-expect (* -3 -2) 6)
(check-expect (* 10 20) 200)
(check-within (* 10.0 20) 200.0 1e-5)
(check-within (* 10 20.0) 200.0 1e-5)
(check-within (* 5.0 15.0) 75.0 1e-5)
(check-within (* 5.0 1e-2) 5e-2 1e-5)
(check-within (* 5.2e10 -1e-5) -520000.0 1e-5)
(check-within (* 1 2 3 4.0 5.0) 120.0 1e-5)

(check-expect (/ 3 1) 3)
(check-expect (/ 10 3) 3)
(check-error (/ 0 0))
(check-error (/ 1 0))

(check-within (/ 10 3.0) 3.3333333 1e-5)
(check-within (/ 10.0 3) 3.3333333 1e-5)
(check-expect (/ 1.0 0.0) (/ 2.0 0.0))
(check-expect (/ -1.0 0.0) (/ -2.0 0.0))
(check-within (/ 5.5 2e3) 0.00275 1e-5)

