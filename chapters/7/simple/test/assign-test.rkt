#lang typed/racket

(require "../base/base.rkt")


(displayln "Start assign test.")


(for ([code
       (in-list
        '(
          (let ([n 0])
            (set! n (+ n 1))
            n)
          (let ([x 0])
            (letrec ([even (λ ()
                             (if (zero? x)
                                 1
                                 (begin
                                   (set! x (- x 1))
                                   (odd))))]
                     [odd  (λ ()
                             (if (zero? x)
                                 0
                                 (begin
                                   (set! x (- x 1))
                                   (even))))])
              (set! x 13)
              (odd)))
          (let* ([g (let ([counter 0])
                      (λ ()
                        (set! counter (- counter -1))
                        counter))]
                 [a (g)]
                 [b (g)])
            (displayln (format "a = ~a, b = ~a" a b)))
          (letrec ([times4 (λ (x)
                             (if (zero? x)
                                 0
                                 (- (times4 (- x 1)) -4)))])
            (displayln (times4 3)))
          (let ([times4 0])
            (set! times4
                  (λ (x)
                    (if (zero? x)
                        0
                        (- (times4 (- x 1)) -4))))
            (displayln (times4 3)))
          (let* ([x 11]
                 [p (λ () x)])
            (displayln (p))
            (set! x 13)
            (displayln (p)))
          (let ([f (λ () 1)])
            (let ([g (λ () (f))])
              (displayln (format "Expected ~a, given ~a." 1 (g)))
              (set! f (λ () 2))
              (displayln (format "Expected ~a, given ~a." 2 (g)))))
          #;(let ([n (read)])
              (displayln n))
          ))])
  (displayln "-----------------------------")
  (pretty-print code)
  (pretty-print (*eval* code (base-env) (end-cont)))
  (newline))
