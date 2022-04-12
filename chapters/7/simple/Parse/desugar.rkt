#lang typed/racket

(require "../types/types.rkt")

(provide desugar)


(: desugar [-> S-Exp S-Exp])
(define desugar
  (let ()
    (define-predicate define-exp? (List* 'define S-Exp S-Exp (Listof S-Exp)))

    (λ (code)
      (match code
        ;; macro
        [`(,(or 'quote 'quasiquote) ,(? literal? atom)) atom]
        [`(,(or 'quote 'quasiquote) ,(? symbol?))       code]
        [`(,(or 'quote 'quasiquote) ())                 'null]
        [`',(? list? ls)
         code
         #;`(list ,@(map (ann (λ (datum) (desugar `',datum)) [-> S-Exp S-Exp]) ls))]
        #;[`',_ code]

        [``,(list 'unquote datum) (desugar datum)]
        [``,(list 'unquote-splicing datum)
         (raise-arguments-error 'unquote-splicing
                                "unquote-splicing: invalid context within quasiquote"
                                "at" (list 'unquote-splicing datum)
                                "in" code)]
        [``,(? list? ls)
         `(list
           ,@(for/foldr : (Listof S-Exp)
                 ([res '()])
                 ([datum (in-list ls)])
               (match datum
                 [(list 'unquote datum) (cons (desugar datum) res)]
                 #;[(list 'unquote-splicing datum) (append (desugar datum) res)] ; TODO
                 [_ (cons (desugar (list 'quasiquote datum)) res)])))]

        [`(begin ,define-exps ...
                 (begin ,exps ..1))
         #:when (and ((listof? define-exp?) define-exps)
                     ((listof? s-exp?) exps))
         (desugar
          `(begin
             ,@(map desugar define-exps)
             ,@exps))]
        [`(begin (define ,vars ,vals) ..1 ,exps ..1)
         #:when (and ((listof? symbol?) vars)
                     ((listof? s-exp?)  vals)
                     ((listof? s-exp?)  exps))
         (desugar
          `(letrec ,(map (ann (λ (var val) `[,var ,val])
                              [-> Symbol S-Exp (List Symbol S-Exp)])
                         vars vals)
             ,@exps))]
        [`(begin ,define-exps ..1 ,exps ..1)
         #:when (and ((listof? define-exp?) define-exps)
                     ((listof? s-exp?) exps))
         (desugar
          `(begin
             ,@(map desugar define-exps)
             ,@exps))]
        [`(begin ,exp)
         #:when (s-exp? exp)
         (desugar exp)]
        [`(begin (begin ,exps1 ..1) ,exps2 ...)
         #:when (and ((listof? s-exp?) exps1)
                     ((listof? s-exp?) exps2))
         (desugar `(begin ,@exps1 ,@exps2))]
        [`(begin ,exps1 ...
                 (begin (define ,vars ,vals) ...
                        ,exps2 ...))
         #:when (and (null? vars)
                     (null? vals)
                     ((listof? s-exp?) exps1)
                     ((listof? s-exp?) exps2))
         (desugar `(begin ,@exps1 ,@exps2))]
        [`(begin
            ,exps1 ...
            ,(or (? literal?)
                 (? symbol?)
                 (? null?)
                 (? undefined?)
                 '(void)
                 `(quote ,_))
            ..1
            ,exps2 ..1)
         #:when (and ((listof? s-exp?) exps1)
                     ((listof? s-exp?) exps2))
         (desugar `(begin ,@exps1 ,@exps2))]

        [`(define (,head ,args ... . ,rests) ,bodys ..1)
         #:when (and (symbol? head)
                     ((listof? symbol?) args)
                     ((or/c symbol? null?) rests)
                     ((listof? s-exp?) bodys))
         `(define ,head ,(desugar `(λ (,@args . ,rests) ,@bodys)))]

        [`(if #t ,exp1 ,exp2)
         #:when (and (s-exp? exp1) (s-exp? exp2))
         (desugar exp1)]
        [`(if #f ,exp1 ,exp2)
         #:when (and (s-exp? exp1) (s-exp? exp2))
         (desugar exp2)]

        [`(when ,pred-exp ,body-exps ..1)
         #:when (and (s-exp? pred-exp)
                     ((listof? s-exp?) body-exps))
         (desugar
          `(if ,pred-exp
               (begin ,@body-exps)
               (void)))]
        [`(unless ,pred-exp ,body-exps ..1)
         #:when (and (s-exp? pred-exp)
                     ((listof? s-exp?) body-exps))
         (desugar
          `(if ,pred-exp
               (void)
               (begin ,@body-exps)))]
        [`(cond [,pred-exp ,body-exp ,body-exp* ...]
                ,next ...)
         #:when (and (s-exp? pred-exp)
                     (s-exp? body-exp)
                     ((listof? s-exp?) body-exp*)
                     ((listof? (listof? s-exp?)) next))
         (desugar
          `(if ,(if (eq? 'else pred-exp)
                    #t
                    pred-exp)
               ,(if (null? body-exp*)
                    body-exp
                    `(begin ,body-exp ,@body-exp*))
               ,(if (null? next)
                    '(void)
                    `(cond ,@next))))]

        [`(and ,exps ...)
         #:when ((listof? s-exp?) exps)
         (if (null? exps)
             #t
             (desugar
              `(if ,(car exps)
                   (and ,@(cdr exps))
                   #f)))]
        [`(or ,exps ...)
         #:when ((listof? s-exp?) exps)
         (if (null? exps)
             #f
             (desugar
              `(if ,(car exps)
                   #t
                   (or ,@(cdr exps)))))]

        ['(amb) '(*amb*)]
        [`(amb ,exp) #:when (s-exp? exp) (desugar exp)]
        [`(amb ,exps ..2)
         #:when ((listof? s-exp?) exps)
         (define amb  (gensym 'amb))
         (define succ (gensym 'succ))
         (define fail (gensym 'fail))
         (desugar
          `(let ([,amb *amb*])
             (let/cc ,succ
               ,@(for/list : (Listof S-Exp)
                           ([exp (in-list exps)])
                   `(let/cc ,fail
                      (set! *amb* (λ () (,fail "amb: branch failed!")))
                      (,succ ,exp)))
               (set! *amb* ,amb)
               (*amb*))))]

        [`(let ([,bind-vars ,bind-exps] ...)
            ,body-exps ..1)
         #:when (and ((listof? symbol?) bind-vars)
                     ((listof? s-exp?)  bind-exps)
                     ((listof? s-exp?)  body-exps))
         (desugar `((λ ,bind-vars ,@body-exps) ,@bind-exps))]
        [`(let ,loop ([,bind-vars ,bind-exps] ...)
            ,body-exps ..1)
         #:when (and (symbol? loop)
                     ((listof? symbol?) bind-vars)
                     ((listof? s-exp?)  bind-exps)
                     ((listof? s-exp?)  body-exps))
         (desugar
          `(let ()
             (define ,loop (λ ,bind-vars ,@body-exps))
             (,loop ,@bind-exps)))]
        [`(let* ([,bind-vars ,bind-exps] ...)
            ,body-exps ..1)
         #:when (and ((listof? symbol?) bind-vars)
                     ((listof? s-exp?)  bind-exps)
                     ((listof? s-exp?)  body-exps))
         (desugar
          (if (and (null? bind-vars) (null? bind-exps))
              `(let () ,@body-exps)
              `(let ([,(car bind-vars) ,(car bind-exps)])
                 (let* ,(map (ann (λ (var exp) `[,var ,exp])
                                  [-> Symbol S-Exp (List Symbol S-Exp)])
                             (cdr bind-vars)
                             (cdr bind-exps))
                   ,@body-exps))))]
        [`(letrec ([,bind-vars : ,bind-types ,bind-exps] ...)
            ,body-exps ..1)
         #:when (and ((listof? symbol?) bind-vars)
                     ((listof? type?)   bind-types)
                     ((listof? s-exp?)  bind-exps)
                     ((listof? s-exp?)  body-exps))
         #;(desugar
            `(let ,(map (ann (λ (var exp) `[,var ,exp])
                             [-> Symbol S-Exp (List Symbol S-Exp)])
                        bind-vars bind-exps)
                 ,@(map desugar body-exps)))
         (desugar
          `(let ,(map (ann (λ (var type) `[,var (cast undefined ,type)])
                           [-> Symbol Type (List Symbol S-Exp)])
                      bind-vars bind-types)
               ,@(map (ann (λ (var exp) `(set! ,var ,exp))
                           [-> Symbol S-Exp (List 'set! Symbol S-Exp)])
                      bind-vars bind-exps)
             ,@(map desugar body-exps)))]

        [`(let/cc ,cc-var1 (let/cc ,cc-var2 ,body-exps ..1))
         #:when (and (symbol? cc-var1)
                     (symbol? cc-var2)
                     ((listof? s-exp?) body-exps))
         (desugar `(let/cc ,cc-var1 (let ([,cc-var2 ,cc-var1]) ,@body-exps)))]

        [`(with-handlers () ,body-exps ..1)
         #:when ((listof? s-exp?) body-exps)
         (desugar `(begin ,@body-exps))]
        [`(with-handlers ([,pred-exps0 ,handler-exps0] ...)
            (with-handlers ([,pred-exps1 ,handler-exps1] ...)
              ,body-exps ..1))
         #:when (and ((listof? s-exp?) pred-exps0)
                     ((listof? s-exp?) handler-exps0)
                     ((listof? s-exp?) pred-exps1)
                     ((listof? s-exp?) handler-exps1)
                     ((listof? s-exp?) body-exps))
         (desugar
          `(with-handlers
             ,(map (ann (λ (pred-exp handler-exp)
                          `[,(desugar pred-exp) ,(desugar handler-exp)])
                        [-> S-Exp S-Exp (List S-Exp S-Exp)])
                   (append pred-exps0 pred-exps1)
                   (append handler-exps0 handler-exps1))
             ,@body-exps))]
        [`(with-handlers ([,pred-exps ,handler-exps] ..1) ,body-exps ..1)
         #:when (and ((listof? s-exp?) pred-exps)
                     ((listof? s-exp?) handler-exps)
                     ((listof? s-exp?) body-exps))
         (define cc     (gensym 'cc))
         (define arg    (gensym 'arg))
         (define raise0 (gensym 'raise))
         (define raise1 (gensym 'raise))
         (desugar
          `(let/cc ,cc
             (let* ([,raise0 raise]
                    [,raise1
                     (λ (,arg)
                       (,cc
                        (cond
                          ,@(map (ann (λ (pred-exp handler-exp)
                                        `[(,pred-exp ,arg) (,handler-exp ,arg)])
                                      [-> S-Exp S-Exp (List S-Exp S-Exp)])
                                 pred-exps handler-exps)
                          [else (,raise0 ,arg)])))])
               (set! raise ,raise1)
               ,@body-exps
               (set! raise ,raise0))))]

        ['(mutex) '(mutex 1)]
        [`(with-mutex ,exp ,body-exps ..1)
         #:when (and (s-exp? exp)
                     ((listof? s-exp?) body-exps))
         (define mut (gensym 'mut))
         (desugar
          `(let ([,mut ,exp])
             (wait ,mut)
             ,@body-exps
             (signal ,mut)))]

        [`(,op ,binds ,body-exps ..2)
         #:when (and (case op
                       [(let/cc lambda λ trace-lambda trace-λ) #t]
                       [else #f])
                     (s-exp? binds)
                     ((listof? s-exp?) body-exps))
         (desugar `(,op ,binds (begin ,@body-exps)))]

        ;; reduce
        [`(,op ,args ,body-exp)
         #:when (and (case op
                       [(lambda λ trace-lambda trace-λ) #t]
                       [else #f])
                     ((or/c symbol? (listof? symbol?)) args)
                     (s-exp? body-exp))
         `(,op ,args ,(desugar body-exp))]
        [`(ann ,exp ,t)
         #:when (and (s-exp? exp) (type? t))
         `(ann ,(desugar exp) ,(desugar-type t))]
        [`(cast ,exp ,t)
         #:when (and (s-exp? exp) (type? t))
         `(cast ,(desugar exp) ,(desugar-type t))]
        [`(inst ,exp ,ts ..1)
         #:when (and (s-exp? exp) ((listof? type?) ts))
         `(inst ,(desugar exp) ,@(map desugar-type ts))]
        [(? list?) (map desugar code)]
        [_ code
           #;(match code
               [(? boolean? bool) code]
               [(? real? num)     code]
               [(? string? str)   code]
               [(? char? ch)      code]

               [(? symbol? var)   code]

               [`(set! ,var ,exp)
                #:when (and (symbol? var) (s-exp? exp))
                `(set! ,var ,(desugar exp))]

               [`(if ,(? s-exp? pred-exp)
                     ,(? s-exp? true-exp)
                     ,(? s-exp? false-exp))
                `(if ,(desugar pred-exp)
                     ,(desugar true-exp)
                     ,(desugar false-exp))]

               [`(with-handlers ([,pred-exps ,handler-exps] ...)
                   ,body-exp)
                #:when (and ((listof? s-exp?) pred-exps)
                            ((listof? s-exp?) handler-exps)
                            (s-exp? body-exp))
                `(with-handlers
                   ,(map (ann (λ (pred-exp handler-exp)
                                `[,(desugar pred-exp) ,(desugar handler-exp)])
                              [-> S-Exp S-Exp (List S-Exp S-Exp)])
                         pred-exps handler-exps)
                   ,(desugar body-exp))]

               [`(letrec ([,bind-vars ,bind-exps] ...) ,body-exp)
                #:when (and ((listof? symbol?) bind-vars)
                            ((listof? s-exp?)  bind-exps)
                            (s-exp? body-exp))
                `(letrec
                     ,(map (ann (λ (bind-var bind-exp) `[,bind-var ,(desugar bind-exp)])
                                [-> Symbol S-Exp (List Symbol S-Exp)])
                           bind-vars bind-exps)
                   ,(desugar body-exp))]

               [`(let/cc ,cc-var ,body-exp)
                #:when (and (symbol? cc-var) (s-exp? body-exp))
                `(let/cc ,cc-var ,(desugar body-exp))]


               [`(,(? λ?) ,args ,body-exp)
                #:when (and ((or/c symbol? (listof? symbol?)) args)
                            (s-exp? body-exp))
                `(λ ,args ,(desugar body-exp))]
               [`(,(? trace-λ?) ,args ,body-exp)
                #:when (and ((or/c symbol? (listof? symbol?)) args)
                            (s-exp? body-exp))
                `(trace-λ ,args ,(desugar body-exp))]

               [`(,op ,exps ...)
                #:when (and (s-exp? op) ((listof? s-exp?) exps))
                (map desugar (cons op exps))]
               )]
        ))))
