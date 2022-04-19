#lang typed/racket

(require "../types/types.rkt"
         "../ExpValues/values-sig.rkt"
         "../Procedure/proc-sig.rkt"
         "../Expressions/exp-sig.rkt"
         "env-sig.rkt")

(provide env@)


(: report-no-binding-found [-> Symbol Nothing])
(define report-no-binding-found
  (λ (search-var)
    (error 'apply-env "No binding for ~s" search-var)))

(: report-invalid-env [-> Env Nothing])
(define report-invalid-env
  (λ (env)
    (error 'apply-env "Bad environment: ~s" env)))


(define-unit env@
  (import values^ proc^ exp^)
  (export env^)


  (: base-env  (Parameter Env))
  (: empty-env [-> Env])
  (define-values (base-env empty-env)
    (let ([empty-environment (make-env 'empty-env #hasheq())])
      (values (make-parameter empty-environment)
              (λ () empty-environment))))

  (: empty-env? [-> Env Boolean])
  (define empty-env? (λ (env) (eqv? (env-type env) 'empty-env)))


  (: extend-env [-> Symbol DenVal Env Env])
  (define extend-env
    (λ (var val saved-env)
      (extend-env-bind var (box val) saved-env)))

  (: extend-env* [-> (Listof Symbol) (Listof DenVal) Env Env])
  (define extend-env*
    (λ (vars vals saved-env)
      (unless (= (length vars) (length vals))
        (raise-arguments-error 'extend-env*
                               "The number of formal arguments and actual arguments is not equal."
                               "formal arguments" vars
                               "actual arguments" vals))

      (make-env 'extend-env
                (for/fold ([res : (Immutable-HashTable Symbol (Boxof DenVal))
                                (env-binds saved-env)])
                          ([var (in-list vars)]
                           [val (in-list vals)])
                  (hash-set res var (box val))))))

  (: extend-env+ [-> (Listof (Pair Symbol DenVal)) Env Env])
  (define extend-env+
    (λ (binds saved-env)
      (make-env 'extend-env
                (for/fold ([res : (Immutable-HashTable Symbol (Boxof DenVal))
                                (env-binds saved-env)])
                          ([bind (in-list binds)])
                  (hash-set res (car bind) (box (cdr bind)))))))

  (: extend-env-bind [-> Symbol (Boxof DenVal) Env Env])
  (define extend-env-bind
    (λ (var ref saved-env)
      (make-env 'extend-env (hash-set (env-binds saved-env) var ref))))

  (: extend-env-bind* [-> (Listof Symbol) (Listof (Boxof DenVal)) Env Env])
  (define extend-env-bind*
    (λ (vars refs saved-env)
      (unless (= (length vars) (length refs))
        (raise-arguments-error 'extend-env-bind*
                               "The number of formal arguments and actual arguments is not equal."
                               "formal arguments" vars
                               "actual arguments" refs))

      (make-env 'extend-env
                (for/fold ([res : (Immutable-HashTable Symbol (Boxof DenVal))
                                (env-binds saved-env)])
                          ([var (in-list vars)]
                           [ref (in-list refs)])
                  (hash-set res var ref)))))

  (: extend-env-bind+ [-> (Listof (Pair Symbol (Boxof DenVal))) Env Env])
  (define extend-env-bind+
    (λ (binds saved-env)
      (make-env 'extend-env
                (for/fold ([res : (Immutable-HashTable Symbol (Boxof DenVal))
                                (env-binds saved-env)])
                          ([bind (in-list binds)])
                  (hash-set res (car bind) (cdr bind))))))


  (: extend-env? [-> Env Boolean])
  (define extend-env? (λ (env) (eqv? (env-type env) 'extend-env)))


  (: env? [-> Any Boolean : Env])
  (define-predicate env? Env)


  (: apply-env-ref [-> Env Symbol (Boxof DenVal)])
  (define apply-env-ref
    (λ (env var)
      (hash-ref (env-binds env) var)))

  (: apply-env [-> Env Symbol DenVal])
  (define apply-env
    (λ (env var)
      (unbox (apply-env-ref env var))))


  (: has-binding? [-> Env Symbol Boolean])
  (define has-binding?
    (λ (env var)
      (hash-has-key? (env-binds env) var)))

  (: set-binding! [-> Env Symbol DenVal Void])
  (define set-binding!
    (λ (env var new-val)
      (set-box! (apply-env-ref env var) new-val)))

  (: copy-env [-> Env Env])
  (define copy-env
    (λ (saved-env)
      (make-env (env-type saved-env)
                (for/hasheq : (Immutable-HashTable Symbol (Boxof DenVal))
                            ([(k v) (in-hash (env-binds saved-env))])
                  (values k (box (unbox v)))))))

  )
