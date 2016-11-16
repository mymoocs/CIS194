#lang lazy

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Exercise 1


;F0 = 0
;F1 = 1
;Fn = Fn−1 + Fn−2 (n ≥ 2)--}
(define fib
  (lambda (n)
  (cond
    [(= n 0) 0]
    [(= n 1) 1]
    [else (+ (fib (- n 1)) (fib (- n 2)))]
    )
  ))

(define iterate
  (lambda (f s)
       (cons s (iterate f (f s)))
       )
  )


(define nats1 (iterate (lambda (x) (+ x 1)) 0))

(define nats
  ;((iterate (lambda (x) (+ x 1)) 0)))
  (cons 0
       (map (lambda (x) (+ x 1)) nats  )
        )
 )
;fibs1 :: [Integer]
(define fibs1
 (map fib nats1))
