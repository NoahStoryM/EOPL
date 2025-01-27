#lang typed/racket

(require "../Parse/parser.rkt")


(pretty-print (parser '2))
(pretty-print (parser '-9))
(pretty-print (parser 'x))
(pretty-print (parser 'i))

(pretty-print (parser '#\a))
(pretty-print (parser '"b"))

(pretty-print (parser '(not #t)))
(pretty-print (parser '(not #f)))

(pretty-print (parser '(minus -9)))
(pretty-print (parser '(minus i)))
(pretty-print (parser '(minus x)))

(pretty-print (parser '(greater? i x)))
(pretty-print (parser '(less? i x)))

(pretty-print (parser '(cons i i)))
(pretty-print (parser '(car (cons i x))))
(pretty-print (parser '(list x i i)))
(pretty-print (parser '(null? (empty-list))))

(pretty-print (parser '(let ([x 1])
                         (cons x x))))

(pretty-print (parser '(let ([a 'a]
                             [b 'b]
                             [c 'c])
                         (list a b c))))


(pretty-print (parser '(if (greater? 2 1) 'true 'false)))

(pretty-print (parser '(cond [(null? (list 1 2 3)) 'cond-1]
                             [(null? (list 9 0 8)) 'cond-2]
                             [else 'else-cons])))


(pretty-print (parser '(let ([x 30])
                         (let* ([x (- x 1)]
                                [y (- x 2)])
                           (+ x y)
                           (* x y)
                           (- x y)))))

(pretty-print (parser '(let* ([a 1]
                              [b 2]
                              [c 3])
                         (list a b c))))

(pretty-print (parser '(let ([x 30])
                         (let* ([x (- x 1)]
                                [y (- x 2)])
                           (- x y)))))


(pretty-print (parser '(let ([f (λ (x) (- x 11))])
                         (f (f 77)))))

(pretty-print (parser ''a))
(pretty-print (parser ''#t))
(pretty-print (parser ''233))
(pretty-print (parser ''"hello"))
(pretty-print (parser ''#\b))
(pretty-print (parser ''(1 2 3 'a "cd")))


(pretty-print (parser '(λ (f)
                         ((λ (recur-func)
                            (recur-func recur-func))
                          (λ (recur-func)
                            (f (λ args
                                 (apply (recur-func recur-func) args))))))))


(pretty-print (parser '(cond [(= n 0) 1]
                             [(= n 1) 1]
                             [else (* n (fact (sub1 n)))])))
(pretty-print (parser '(λ (fact)
                         (λ (n)
                           (cond [(= n 0) 1]
                                 [(= n 1) 1]
                                 [else (* n (fact (sub1 n)))])))))


(pretty-print (parser '(let ([funcs
                              (Y*
                               (λ (even? odd?)
                                 (λ (num)
                                   (cond [(zero? num) #t]
                                         [(= 1 num) #f]
                                         [else (odd? (- num 1))])))
                               (λ (even? odd?)
                                 (λ (num)
                                   (cond [(zero? num) #f]
                                         [(= 1 num) #t]
                                         [else (even? (- num 1))]))))])
                         (let ([even? (car funcs)]
                               [odd?  (car (cdr funcs))])
                           (displayln (eq? #t (even? 0)))))))


(pretty-print (parser '(λ funcs
                         ((λ (recur-funcs)
                            (displayln recur-funcs)
                            (displayln (recur-funcs recur-funcs))

                            (recur-funcs recur-funcs))
                          (λ (recur-funcs)
                            (map (λ (func)
                                   (λ args
                                     (apply (apply func (recur-funcs recur-funcs)) args)))
                                 funcs))))))


(pretty-print (parser '(if #t 1 2)))


(pretty-print (parser '(letrec ([even? (λ (num)
                                         (cond [(zero? num) #t]
                                               [(= 1 num) #f]
                                               [else (odd? (sub1 num))]))]
                                [odd?  (λ (num)
                                         (cond [(zero? num) #f]
                                               [(= 1 num) #t]
                                               [else (even? (sub1 num))]))])
                         (odd? 5))))

(pretty-print (parser '(letrec ([fib (λ (num)
                                       (cond [(= 0 num) 0]
                                             [(= 1 num) 1]
                                             [else (+ (fib (- num 1))
                                                      (fib (- num 2)))]))])
                         (fib 0))))


(pretty-print (parser '(let ([x 0])
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
                           (odd)))))

(pretty-print (parser '(let* ([g (let ([counter 0])
                                   (λ ()
                                     (set! counter (- counter -1))
                                     counter))]
                              [a (g)]
                              [b (g)])
                         (displayln (format "a = ~a, b = ~a" a b)))))

(pretty-print (parser '(let ([n 0])
                         (set! n (+ n 1))
                         n)))

(pretty-print (parser '(let ([n 1])
                         (with-handlers ([(λ (arg) (eq? arg 0))
                                          (λ (arg) (displayln "raise a value: ZERO."))]
                                         [(λ (arg) (eq? arg 1))
                                          (λ (arg) (displayln "raise a value: ONE."))])
                           (raise n)))))
