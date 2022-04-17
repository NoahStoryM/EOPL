#lang typed/racket

(require "../types/types.rkt"
         "renv-sig.rkt")

(provide renv@)


(: report-no-relation-binding-found [-> Type Nothing])
(define report-no-relation-binding-found
  (λ (search-type)
    (error 'apply-env "No relation binding for ~s" search-type)))

(: report-invalid-renv [-> REnv Nothing])
(define report-invalid-renv
  (λ (renv)
    (error 'apply-env "Bad relation environment: ~s" renv)))


(define-unit renv@
  (import)
  (export renv^)


  (: base-renv  (Parameter REnv))
  (: empty-renv [-> REnv])
  (define-values (base-renv empty-renv)
    (let ([empty-relation-environment (make-renv 'empty-renv #hasheq())])
      (values (make-parameter empty-relation-environment)
              (λ () empty-relation-environment))))

  (: empty-renv? [-> REnv Boolean])
  (define empty-renv? (λ (renv) (eqv? (renv-type renv) 'empty-renv)))


  (: extend-renv [-> Type Type REnv REnv])
  (define extend-renv
    (λ (key val saved-renv)
      (make-renv 'extend-renv (hash-set (renv-binds saved-renv) key val))))

  (: extend-renv* [-> (Listof Type) (Listof Type) REnv REnv])
  (define extend-renv*
    (λ (keys vals saved-renv)
      (unless (= (length keys) (length vals))
        (raise-arguments-error 'extend-renv*
                               "The number of formal arguments and actual arguments is not equal."
                               "formal arguments" keys
                               "actual arguments" vals))

      (make-renv 'extend-renv
                 (for/fold ([res : (Immutable-HashTable Type Type)
                                 (renv-binds saved-renv)])
                           ([key (in-list keys)]
                            [val (in-list vals)])
                   (hash-set res key val)))))

  (: extend-renv+ [-> (Listof (Pair Type Type)) REnv REnv])
  (define extend-renv+
    (λ (binds saved-renv)
      (make-renv 'extend-renv
                 (for/fold ([res : (Immutable-HashTable Type Type)
                                 (renv-binds saved-renv)])
                           ([bind (in-list binds)])
                   (hash-set res (car bind) (cdr bind))))))


  (: extend-renv? [-> REnv Boolean])
  (define extend-renv? (λ (renv) (eqv? (renv-type renv) 'extend-renv)))


  (: renv? [-> Any Boolean : REnv])
  (define-predicate renv? REnv)


  (: apply-renv [-> REnv Type Type])
  (define apply-renv
    (λ (renv key)
      (hash-ref (renv-binds renv) key)))


  (: has-rbinding? [-> REnv Type Boolean])
  (define has-rbinding?
    (λ (renv key)
      (hash-has-key? (renv-binds renv) key)))

  )
