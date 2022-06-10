#lang typed/racket

(require "../Parse/parse.rkt")
(require "../Modules/thread.rkt"
         "../Modules/exit.rkt")

(displayln "Start desugar test.\n")


(for ([code
       (in-list
        '(
          #;(let* ([x (- x 1)]
                   [y (- x 2)])
              (+ x y)
              (* x y)
              (- x y))
          #;(let ([x 30])
              (let* ([x (- x 1)]
                     [y (- x 2)])
                (+ x y)
                (* x y)
                (- x y)))
          #;(let* ([*amb* (λ () (error 'amb "Amb tree exhausted!"))]
                   [assert (λ (p) (if p p (amb)))])
              (if (amb #f #t)
                  1
                  (amb)))
          #;(let* ([*amb* (λ () (error 'amb "Amb tree exhausted!"))]
                   [assert (λ (p) (if p p (amb)))])
              (let ([x (list (amb 2 1 -2 5 8 18) (amb 9 8 2 4 14 20))])
                (assert (> (car x) (cadr x)))
                (displayln x)))
          (begin
            (define fact
              (λ (n)
                (if (zero? n)
                    1
                    (* n (fact (sub1 n))))))
            (displayln (fact 0))
            (displayln (fact 1))
            (displayln (fact 2))
            (displayln (fact 3)))
          ))])
  (displayln "----------------------------------")
  (displayln "\n Initial code:")
  (pretty-print code)
  (displayln "\n After desugar:")
  (pretty-print (desugar
                 code))
  (displayln "\n After auto-apply:")
  (pretty-print (auto-apply
                 (desugar
                  code)))
  (displayln "\n After insert-modules:")
  (pretty-print (module/exit
                 (module/thread
                  (auto-apply
                   (desugar
                    code))
                  1)))
  (displayln "\n After auto-cps:")
  (pretty-print (auto-cps
                 (desugar
                  (module/exit
                   (module/thread
                    (auto-apply
                     (desugar
                      code))
                    1)))))
  #;(pretty-print (auto-cps (desugar code)))
  #;(pretty-print (desugar (auto-cps (desugar code))))
  (newline))
