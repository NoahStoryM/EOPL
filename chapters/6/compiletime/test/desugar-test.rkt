#lang typed/racket

(require "../Parse/parse.rkt")

(displayln "Start desugar test.\n")


(for ([code
       (in-list
        '(
          (let* ([x (- x 1)]
                 [y (- x 2)])
            (+ x y)
            (* x y)
            (- x y))
          (let ([x 30])
            (let* ([x (- x 1)]
                   [y (- x 2)])
              (+ x y)
              (* x y)
              (- x y)))
          (let* ([*amb* (λ () (error 'amb "Amb tree exhausted!"))]
                 [assert (λ (p) (if p p (amb)))])
            (if (amb #f #t)
                1
                (amb)))
          (let* ([*amb* (λ () (error 'amb "Amb tree exhausted!"))]
                 [assert (λ (p) (if p p (amb)))])
            (let ([x (list (amb 2 1 -2 5 8 18) (amb 9 8 2 4 14 20))])
              (assert (> (car x) (cadr x)))
              (displayln x)))
          ))])
  (displayln "----------------------------------")
  (pretty-print code)
  (pretty-print (desugar code))
  (pretty-print (auto-cps (desugar code)))
  (pretty-print (desugar (auto-cps (desugar code))))
  (newline))
