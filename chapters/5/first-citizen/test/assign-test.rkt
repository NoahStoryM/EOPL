#lang typed/racket

(require "../base/base.rkt")


(for ([code
       (in-list
        '(
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
              (displayln (odd))))
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
          (begin
            (define swap!
              (λ (x y)
                (define t (unbox x))
                (set-box! x (unbox y))
                (set-box! y t)))
            (define a 123)
            (define b 456)
            (displayln (format "Before swap: a = ~a, b = ~a" a b))
            (swap! &a &b)
            (displayln (format "After  swap: a = ~a, b = ~a" a b)))
          (begin
            (define swap!
              (λ (x y)
                (define t *x)
                (set-box! x *y)
                (set-box! y t)))
            (define a 123)
            (define b 456)
            (displayln (format "Before swap: a = ~a, b = ~a" a b))
            (swap! &a &b)
            (displayln (format "After  swap: a = ~a, b = ~a" a b)))
          (begin
            (define swap!
              (λ (&x &y)
                (define t x)
                (set! x y)
                (set! y t)))
            (define a 123)
            (define b 456)
            (displayln (format "Before swap: a = ~a, b = ~a" a b))
            (swap! &a &b)
            (displayln (format "After  swap: a = ~a, b = ~a" a b)))
          ((λ (a)
             ((λ (&b)
                (displayln (format "Before: a = ~a, b = ~a" a b))
                (set! b -1)
                (displayln (format "After : a = ~a, b = ~a" a b)))
              &a)) 1)
          (let ([a 1])
            ((λ (&b)
               (displayln (format "Before: a = ~a, b = ~a" a b))
               (set! b -1)
               (displayln (format "After : a = ~a, b = ~a" a b)))
             &a))
          (let ([a 1])
            (let ([&b &a])
              (displayln (format "Before: a = ~a, b = ~a" a b))
              (set! b -1)
              (displayln (format "After : a = ~a, b = ~a" a b))))
          (let* ([a 1] [&b &a])
            (displayln (format "Before: a = ~a, b = ~a" a b))
            (set! b -1)
            (displayln (format "After : a = ~a, b = ~a" a b)))

          #;(let ([n (read)])
              (displayln n))
          ))])
  (*eval* code (base-env) (end-cont)))
