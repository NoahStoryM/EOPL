#lang typed/racket

(require "../types/types.rkt"
         "subst-sig.rkt")

(provide subst@)


(define-unit subst@
  (import)
  (export subst^)

  (: empty-subst [-> Subst])
  (define empty-subst (λ () #hasheq()))

  (: extend-subst [-> Subst Symbol Type Subst])
  (define extend-subst
    (λ (s tv t)
      (hash-set
       (for/hasheq : Subst ([(lhs rhs) (in-hash s)])
         (values lhs (apply-one-subst rhs tv t)))
       tv t)))


  (: apply-one-subst [-> Type Symbol Type Type])
  (define apply-one-subst
    (λ (t0 tv t1)
      (: apply-t [-> Type Type])
      (define apply-t (λ (t0) (apply-one-subst t0 tv t1)))

      (match t0
        [(or 'Boolean 'Real 'Char 'String 'Bytes 'Keyword 'Symbol) t0]
        [`(Values ,ts ...)
         #:when ((listof? type?) ts)
         `(Values ,@(map apply-t ts))]
        [`[-> ,I ,O : #:+ ,T #:- ,F]
         `[-> ,(apply-t I) ,(apply-t O) : #:+ ,T #:- ,F]]
        [(? symbol?) (if (eq? t0 tv) t1 t0)])))

  (: apply-subst-to-type [-> Type Subst Type])
  (define apply-subst-to-type
    (λ (t s)
      (: apply-t [-> Type Type])
      (define apply-t (λ (t) (apply-subst-to-type t s)))

      (match t
        [(or 'Boolean 'Real 'Char 'String 'Bytes 'Keyword 'Symbol) t]
        [`(Values ,ts ...)
         #:when ((listof? type?) ts)
         `(Values ,@(map apply-t ts))]
        [`[-> ,I ,O : #:+ ,T #:- ,F]
         `[-> ,(apply-t I) ,(apply-t O) : #:+ ,T #:- ,F]]
        [(? symbol?) (or (hash-ref s t #f) t)])))

  )
