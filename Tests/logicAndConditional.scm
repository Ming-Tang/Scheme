(check-expect (not #f) #t)
(check-expect (not #t) #f)
(check-expect (not 0) #f)
(check-expect (not "abc") #f)

(check-expect (and #f #f) #f)
(check-expect (and #f #t) #f)
(check-expect (and #t #f) #f)
(check-expect (and #t #t) #t)
(check-expect (and #f #f #f (/ 1 0)) #f)
(check-expect (and 10 #f) #f)
(check-error (and #t (/ 1 0)))

(check-expect (or #f #f) #f)
(check-expect (or #t #f) #t)
(check-expect (or #f #t) #t)
(check-expect (or #t #t) #t)
(check-expect (or #f #f 0 (/ 1 0)) 0)
(check-expect (or 10 #f) 10)
(check-expect (or #t (/ 1 0)) #t)

(check-expect (if #t 1 0) 1)
(check-expect (if #f 1 0) 0)
(check-expect (if #t 1 (error "fail")) 1)
(check-expect (if #f (error "fail") 1) 1)

(check-expect (cond [#t 1]) 1)
(check-expect (cond [#f 0] [#t 1]) 1)
(check-expect (cond [#f 0] [#f 1]) (void))
(check-expect (cond [#f 0] [#f 1] [#t 2] [else 3]) 2)
(check-expect (cond [#f 0] [#f 1] [#f 2] [else 3]) 3)

(check-expect (case 0 [(0) 1]) 1)
(check-expect (case 'abc [else 1]) 1)
(check-expect (case 'abc [(abc) 1]) 1)
(check-expect (case 'ghi
                [(abc) 1]
                [(def) 2])
              (void))
(check-expect (case 'ghi
                [(abc) 1]
                [(def) 2]
                [(ghi) 3])
              3)
(check-expect (case 'jkl
                [(abc) 1]
                [(def) 2]
                [(ghi) 3]
                [else 4])
              4)
(check-expect (case 'jkl
                [(abc def ghi) 1]
                [(ghi jkl mno) 2]
                [else 3])
              2)
