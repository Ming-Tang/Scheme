(check-expect (string-length "") 0)
(check-expect (string-length "Hello, world!") 13)

(check-error (string-ref "" 0))
(check-expect (string-ref "Hello" 1) "e")
(check-error (string-ref "Hello" 5))

(check-expect (string-append) "")
(check-expect (string-append "" "" "") "")
(check-expect (string-append "Hello" ", " "world!") "Hello, world!")

(check-expect (substring "" 0 0) "")
(check-expect (substring "arduous" 2 5) "duo")
(check-error (substring "arduous" 2 8))

