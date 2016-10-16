#lang racket

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Exercise 1

; convert positive Integers to a list of digits
(define toDigits
  (lambda (n)
    (if (<= n 0)
        '()
        (append (digitsExceptLast n) (list (remainder n 10)))
    )))

(define  digitsExceptLast
  (lambda (n)
    (toDigits (quotient n 10))
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Exercise 2

; doouble every other sarting from right
(define (doubleEveryOther lst)
  (define (doubleEven xs)
      (cond
        [(empty? xs) '()]
        [else (cons (first xs) (doubleOdd (rest xs)))])
      )

  (define (doubleOdd xs)
    (cond
      [(empty? xs) '()]
      [(cons (* 2 (first xs)) (doubleEven (rest xs)))])
    )

  (cond
    [(even? (length lst)) (doubleOdd lst)]
    [else (doubleEven lst)])
  )
