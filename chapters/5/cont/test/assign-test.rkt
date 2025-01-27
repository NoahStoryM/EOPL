#lang typed/racket

(require "../base/base.rkt")


(*eval* '(let ([n 0])
           (set! n (+ n 1))
           n)
        (base-env)
        (end-cont))


(*eval*
 '(let ([x 0])
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
 (base-env)
 (end-cont))


(*eval*
 '(let* ([g (let ([counter 0])
              (λ ()
                (set! counter (- counter -1))
                counter))]
         [a (g)]
         [b (g)])
    (displayln (format "a = ~a, b = ~a" a b)))
 (base-env)
 (end-cont))


(*eval*
 '(letrec ([times4 (λ (x)
                     (if (zero? x)
                         0
                         (- (times4 (- x 1)) -4)))])
    (displayln (times4 3)))
 (base-env)
 (end-cont))


(*eval*
 '(let ([times4 0])
    (set! times4
      (λ (x)
        (if (zero? x)
            0
            (- (times4 (- x 1)) -4))))
    (displayln (times4 3)))
 (base-env)
 (end-cont))


(*eval*
 '(let* ([x 11]
         [p (λ () x)])
    (displayln (p))
    (set! x 13)
    (displayln (p)))
 (base-env)
 (end-cont))


;; (*eval*
;;  '(let ([n (read)])
;;     (displayln n))
;;  (base-env)
;;  (end-cont))
