#lang typed/racket

(require "../types/types.rkt")

(provide (all-defined-out))


(: auto-ann [-> S-Exp S-Exp])
(define auto-ann
  (λ (code)
    (match code
      [`(ann ,exp ,type)
       #:when (and (s-exp? exp) (type? type))
       `(ann ,(auto-ann exp) ,type)]

      [`(quote ,(? symbol?))  code]
      [`(quote ,(? boolean?)) code]
      [`(quote ,(? real?))    code]
      [`(quote ,(? string?))  code]
      [`(quote ,(? char?))    code]
      [`(quote ,(? s-list? ls)) code]

      [(? boolean?) code]
      [(? real?)    code]
      [(? string?)  code]
      [(? char?)    code]

      [(? symbol?)  code]

      [`(set! ,var ,exp)
       #:when (and (symbol? var) (s-exp? exp))
       `(set! ,var ,(auto-ann exp))]

      [`(begin ,exps ..1)
       #:when ((listof? s-exp?) exps)
       `(begin
          ,@(let desugar : (Listof S-Exp)
                 ([exps exps])
              (match exps
                [`((: ,var ,type)
                   (define ,var ,val)
                   ,exps0 ..1)
                 #:when (and (type?   type)
                             (symbol? var)
                             (s-exp?  val)
                             ((listof? s-exp?) exps0))
                 `((define ,var (ann ,(auto-ann val) ,type))
                   ,@(desugar exps0))]
                [`((define ,var ,val)
                   ,exps0 ..1)
                 #:when (and (symbol? var)
                             (s-exp? val)
                             ((listof? s-exp?) exps0))
                 `((define ,var (ann ,(auto-ann val) ,(guess-type val)))
                   ,@(desugar exps0))]

                [`((: ,var ,type)
                   (define (,head . ,args) ,bodys ..1)
                   ,exps0 ..1)
                 #:when (and (type?   type)
                             (symbol? var)
                             ((or/c symbol? (and/c pair? (listof? symbol?))) head)
                             ((or/c (listof? symbol?) symbol?) args)
                             ((listof? s-exp?) bodys)
                             ((listof? s-exp?) exps0))
                 (desugar
                  `((: ,var ,type)
                    (define ,head (λ ,args . ,bodys))
                    ,@exps0))]
                [`((define (,head . ,args) ,bodys ..1)
                   ,exps0 ..1)
                 #:when (and ((or/c symbol? (and/c pair? (listof? symbol?))) head)
                             ((or/c (listof? symbol?) symbol?) args)
                             ((listof? s-exp?) bodys)
                             ((listof? s-exp?) exps0))
                 (desugar
                  `((define ,head (λ ,args . ,bodys))
                    ,@exps0))]

                [_ (map auto-ann exps)])))]

      [`(if ,pred-exp ,true-exp ,false-exp)
       #:when (and (s-exp? pred-exp)
                   (s-exp? true-exp)
                   (s-exp? false-exp))
       `(if ,(auto-ann pred-exp)
            ,(auto-ann true-exp)
            ,(auto-ann false-exp))]
      [`(cond [,pred-exps ,body-exps ..1]
              ..1)
       #:when (and ((listof? s-exp?) pred-exps)
                   ((and/c pair? (listof? (and/c pair? (listof? s-exp?)))) body-exps))
       `(cond ,@(map (ann (λ (pred-exp body-exps)
                            `[,(auto-ann pred-exp)
                              ,@(map auto-ann body-exps)])
                          [-> S-Exp S-List (Pair S-Exp S-List)])
                     pred-exps body-exps))]

      [`(and ,exps ...) #:when ((listof? s-exp?) exps) `(and ,@(map auto-ann exps))]
      [`(or  ,exps ...) #:when ((listof? s-exp?) exps) `(or  ,@(map auto-ann exps))]

      [`(with-handlers ([,pred-exps ,handler-exps] ...) ,body-exps ..1)
       #:when (and ((listof? s-exp?) pred-exps)
                   ((listof? s-exp?) handler-exps)
                   ((listof? s-exp?) body-exps))
       `(with-handlers ,(map (ann (λ (pred-exp handler-exp)
                                    `[,(auto-ann pred-exp)
                                      ,(auto-ann handler-exp)])
                                  [-> S-Exp S-Exp (List S-Exp S-Exp)])
                             pred-exps handler-exps)
          ,@(map auto-ann body-exps))]
      [`(mutex ,exp) #:when (s-exp? exp) `(mutex ,(auto-ann exp))]
      ['(mutex) code]
      [`(with-mutex ,exp ,body-exps ..1)
       #:when (and (s-exp? exp)
                   ((listof? s-exp?) body-exps))
       `(with-mutex ,(auto-ann exp)
          ,@(map auto-ann body-exps))]

      [`(,let-op ,binds ,body-exps ..1)
       #:when (and (case let-op
                     [(let let* letrec letrec*) #t]
                     [else #f])
                   ((listof? s-exp?) binds)
                   ((listof? s-exp?) body-exps))
       `(,let-op
         ,(for/list : (Listof (List Symbol S-Exp))
                    ([bind (in-list binds)])
            (match bind
              [`[,bind-var : ,bind-type ,bind-exp]
               #:when (and (type?   bind-type)
                           (symbol? bind-var)
                           (s-exp?  bind-exp))
               `[,bind-var (ann ,(auto-ann bind-exp) ,bind-type)]]
              [`[,bind-var ,bind-exp]
               #:when (and (symbol? bind-var)
                           (s-exp?  bind-exp))
               `[,bind-var (ann ,(auto-ann bind-exp) ,(guess-type bind-exp))]]))
         ,@(map auto-ann body-exps))]

      [`(let/cc ,cc-var ,body-exps ..1)
       #:when (and (symbol? cc-var) ((listof? s-exp?) body-exps))
       `(let/cc ,cc-var ,@(map auto-ann body-exps))]


      [`(,(and λ-op (or (? λ?) (? trace-λ?))) ,args ,body-exps ..1)
       #:when (and ((or/c symbol? (listof? symbol?)) args)
                   ((listof? s-exp?) body-exps))
       `(,λ-op ,args ,@(map auto-ann body-exps))]

      [`(,op ,exps ...)
       #:when (and (s-exp? op) ((listof? s-exp?) exps))
       `(,(auto-ann op) ,@(map auto-ann exps))]

      )))
