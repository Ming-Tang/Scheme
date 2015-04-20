(define empty-env (new-env))
(define env-ab (new-env))
(env-set! env-ab 'a 10)
(env-set! env-ab 'b 20)

; When the env is overwritten by empty-env and env-ab,
; the standard library disppears
(define (get-ab) `(,a . ,b))

(check-expect (env-has-parent? empty-env) false)
(check-error (get-ab))
(check-expect ((set-env get-ab env-ab)) '(10 . 20))

(check-expect (env-contains? empty-env 'a) false)
(check-expect (env-contains? env-ab 'a) true)
(check-error (env-get empty-env 'a))
(check-expect (env-get env-ab 'a) 10)
(check-expect
  (let ([env (extend-env env-ab)])
    (env-set! env 'c 1)
    (env-unset! env 'c)
    (env-contains? env 'c))
  false)
(check-expect (env-symbols empty-env) '())
(check-expect (env-symbols env-ab) '(a b))
(check-expect (env-count empty-env) 0)
(check-expect (env-count env-ab) 2)
