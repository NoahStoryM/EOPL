#lang typed/racket

(require "../base/base.rkt")

(displayln "Start amb test.\n")


(for ([code
       (in-list
        '(
          (if (amb #f #t)
              1
              (amb))
          (let ([assert (λ (p) (if p p (amb)))])
            (let ([x (list (amb 2 1 -2 5 8 18) (amb 9 8 2 4 14 20))])
              (assert (> (car x) (cadr x)))
              (pretty-print x)))))])
  (displayln "----------------------------------")
  (pretty-print code)
  (displayln (*eval* code (base-env) (id-cont)))
  (newline))
