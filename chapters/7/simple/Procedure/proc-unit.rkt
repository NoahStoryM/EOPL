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
      (make-proc vars
                 body
                 (extend-env-bind+ (free-binds (if (symbol? vars) (list vars) vars)
                                               body env)
                                   (empty-env))
                 )))


  (: trace-proc? [-> Any Boolean : Trace-Proc])
  (define-predicate trace-proc? Trace-Proc)

  (: trace-procedure [-> (U Symbol (Listof Symbol)) Exp Env Trace-Proc])
  (define trace-procedure
    (λ (vars body env)
      (make-trace-proc vars
                       body
                       (extend-env-bind+ (free-binds (if (symbol? vars) (list vars) vars)
                                                     body env)
                                         (empty-env))
                       )))

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

      (value-of/k (proc-body proc)
                  (if (symbol? vars)
                      (extend-env  vars vals
                                   (proc-saved-env proc))
                      (extend-env* vars vals
                                   (proc-saved-env proc)))
                  (cons (frame 'apply-procedure-frame
                               (ann (λ (cont)
                                      (λ (result)
                                        (when (trace-proc? proc)
                                          (displayln (format "result: ~a\n" result)))
                                        (apply-cont cont result)))
                                    [-> Cont [-> ExpVal FinalAnswer]]))
                        cont))))


  (: free-binds [-> (Listof Symbol) Exp Env (Listof (Pair Symbol Ref))])
  (define free-binds
    (λ (vars exp env)
      (match exp
        [(ann-exp exp type) (free-binds vars exp env)]

        [(assign-exp var exp)
         (free-binds vars (begin-exp (list (var-exp var) exp)) env)]

        [(or (quote-exp _)
             (symbol-exp _)
             (real-exp _)
             (bool-exp _)
             (char-exp _)
             (string-exp _))
         '()]

        [(var-exp var)
         (if (memq var vars)
             '()
             (list (cons var (apply-env-ref env var))))]

        [(begin-exp exps)
         (define curr-free-binds (free-binds vars (car exps) env))
         (define next-exps (cdr exps))
         (if (null? next-exps)
             curr-free-binds
             (append curr-free-binds
                     (free-binds (append (map (inst car Symbol Ref) curr-free-binds) vars)
                                 (begin-exp next-exps)
                                 env)))]

        [(if-exp pred-exp true-exp false-exp)
         (free-binds vars (begin-exp (list pred-exp true-exp false-exp)) env)]

        [(new-closure-exp exp)
         (free-binds vars exp env)]
        [(proc-exp proc-vars body)
         (free-binds (if (symbol? proc-vars)
                         (cons proc-vars vars)
                         (append proc-vars vars))
                     body
                     env)]
        [(call-exp rator rands)
         (free-binds vars
                     (begin-exp (if (var-exp? rands)
                                    (list rator rands)
                                    (cons rator rands)))
                     env)])))

  )