#lang typed/racket

(require "../types/types.rkt"
         "../Continuation/cont-sig.rkt"
         "../Expressions/exp-sig.rkt"
         "../Environment/env-sig.rkt"
         "proc-sig.rkt")

(provide proc@)


(define-unit proc@
  (import cont^ env^ exp^)
  (export proc^)

  (: proc? [-> Any Boolean : Proc])
  (define-predicate proc? Proc)

  (: procedure [-> (U Symbol (Listof Symbol)) Exp Env Proc])
  (define procedure
    (λ (vars body env)
      (make-proc
       vars body
       (extend-env-bind+
        (free-binds
         (if (symbol? vars)
             (list vars)
             (for/list : (Listof Symbol)
                       ([var (in-list vars)])
               (define-values (1st oth) (desugar-variable var))
               (case 1st
                 [(#\&) oth]
                 [else var])))
         body env)
        (empty-env)))))


  (: trace-proc? [-> Any Boolean : Trace-Proc])
  (define-predicate trace-proc? Trace-Proc)

  (: trace-procedure [-> (U Symbol (Listof Symbol)) Exp Env Trace-Proc])
  (define trace-procedure
    (λ (vars body env)
      (make-trace-proc
       vars body
       (extend-env-bind+
        (free-binds
         (if (symbol? vars)
             (list vars)
             (for/list : (Listof Symbol)
                       ([var (in-list vars)])
               (define-values (1st oth) (desugar-variable var))
               (case 1st
                 [(#\&) oth]
                 [else var])))
         body env)
        (empty-env)))))

  (: apply-procedure/k [-> Proc (Listof DenVal) Cont FinalAnswer])
  (define apply-procedure/k
    (λ (proc vals cont)
      (: vars (U Symbol (Listof Symbol)))
      (define vars (proc-vars proc))

      (when (trace-proc? proc)
        (displayln (format "enter: ~a\n"
                           (if (symbol? vars)
                               (cons vars vals)
                               (map (ann (λ (var val) (cons var val))
                                         [-> Symbol DenVal (Pair Symbol DenVal)])
                                    vars vals)))))

      (value-of/k
       (proc-body proc)
       (if (symbol? vars)
           (extend-env vars vals (proc-saved-env proc))
           (extend-env-bind+
            (for/list : (Listof (Pair Symbol (Boxof DenVal)))
                      ([var (in-list vars)]
                       [val (in-list vals)])
              (cond [(denbox? val)
                     (define-values (1st oth) (desugar-variable var))
                     (case 1st
                       [(#\&) (cons oth val)]
                       [else (cons var ((inst box DenVal) val))])]
                    [else (cons var ((inst box DenVal) val))]))
            (proc-saved-env proc)))
       (cons
        (frame
         'apply-procedure-frame
         (inherit-handlers-cont cont)
         (ann (λ (cont)
                (λ (result)
                  (when (trace-proc? proc)
                    (displayln (format "result: ~a\n" result)))
                  (apply-cont cont result)))
              [-> Cont [-> ExpVal FinalAnswer]]))
        cont))))


  (: free-binds [-> (Listof Symbol) Exp Env (Listof (Pair Symbol (Boxof DenVal)))])
  (define free-binds
    (λ (vars exp env)
      (match exp
        [(assign-exp var exp)
         (free-binds vars (begin-exp (list (var-exp var) exp)) env)]

        [(or (symbol-exp _)
             (real-exp _)
             (bool-exp _)
             (char-exp _)
             (string-exp _))
         '()]

        [(var-exp var)
         (define-values (1st oth) (desugar-variable var))
         (case 1st
           [(#\*) (free-binds vars (var-exp oth) env)]
           [else
            (define v
              (case 1st
                [(#\&) oth]
                [else var]))
            (if (memq v vars)
                '()
                (list (cons v (apply-env-ref env v))))])]

        [(begin-exp exps)
         (define curr-free-binds (free-binds vars (car exps) env))
         (define next-exps (cdr exps))
         (if (null? next-exps)
             curr-free-binds
             (append curr-free-binds
                     (free-binds (append (map (inst car Symbol (Boxof DenVal)) curr-free-binds) vars)
                                 (begin-exp next-exps)
                                 env)))]

        [(if-exp pred-exp true-exp false-exp)
         (free-binds vars
                     (begin-exp (list pred-exp true-exp false-exp))
                     env)]
        [(cond-exp branches)
         (free-binds vars
                     (begin-exp (apply append branches))
                     env)]

        [(let-exp bind-vars bind-exps body)
         (free-binds vars
                     (if (or (null? bind-vars) (null? bind-exps))
                         body
                         (call-exp (proc-exp bind-vars body) bind-exps))
                     env)]
        [(letrec-exp bind-vars bind-exps body)
         (define new-env
           (extend-env+ (map (ann (λ (var) (cons var undefined))
                                  [-> Symbol (Pair Symbol Undefined)])
                             vars)
                        env))

         (free-binds (append vars bind-vars) (begin-exp (cons body bind-exps)) new-env)]

        [(let/cc-exp cc-var body) (free-binds (cons cc-var vars) body env)]
        [(handlers-exp catch-preds catch-handlers body)
         (if (or (null? catch-preds) (null? catch-handlers))
             (free-binds vars body env)
             (free-binds vars
                         (begin-exp (cons body (append catch-preds catch-handlers)))
                         env))]
        [(raise-exp  exp) (free-binds vars exp env)]
        [(spawn-exp  exp) (free-binds vars exp env)]
        [(mutex-exp  exp) (free-binds vars exp env)]
        [(wait-exp   exp) (free-binds vars exp env)]
        [(signal-exp exp) (free-binds vars exp env)]
        [(kill-exp   exp) (free-binds vars exp env)]

        [(send-exp tid-exp value-exp)
         (free-binds vars
                     (begin-exp (list tid-exp value-exp))
                     env)]

        [(receive-exp)     '()]
        [(try-receive-exp) '()]
        [(yield-exp)       '()]

        [(proc-exp proc-vars body)
         (free-binds (if (symbol? proc-vars)
                         (cons proc-vars vars)
                         (for/fold ([vars : (Listof Symbol) vars])
                                   ([var (in-list proc-vars)])
                           (define-values (1st oth) (desugar-variable var))
                           (case 1st
                             [(#\&) (cons oth vars)]
                             [else  (cons var vars)])))
                     body env)]
        [(call-exp rator rands)
         (free-binds vars
                     (begin-exp (if (var-exp? rands)
                                    (list rator rands)
                                    (cons rator rands)))
                     env)])))

  )
