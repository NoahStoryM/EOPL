#lang typed/racket

(require "../types/types.rkt"
         "subst-sig.rkt")

(provide subst@)


(: raise-no-occurrence-error [-> Type Type Exp Nothing])
(define raise-no-occurrence-error
  (λ (t1 t2 in)
    (raise-arguments-error
     'no-occurrence? "no-occurrence invariant"
     "t1" t1
     "t2" t2
     "in" in)))


(define-unit subst@
  (import)
  (export subst^)

  (: empty-subst [-> Subst])
  (define empty-subst (λ () (make-hasheq)))

  (: extend-subst! [-> Subst Tvar Type Void])
  (define extend-subst! (λ (s tv t) (hash-set! s tv t)))

  (: safe-extend-subst! [-> Subst Tvar Type Exp Void])
  (define safe-extend-subst!
    (λ (s t1 t2 exp)
      (if (no-occurrence? t1 t2)
          (extend-subst! s t1 t2)
          (raise-no-occurrence-error t1 t2 exp))))


  (: no-occurrence? [-> Tvar Type Boolean])
  (define no-occurrence?
    (λ (tv t)
      (: no-occur-tv? [-> Type Boolean])
      (define no-occur-tv? (λ (t) (no-occurrence? tv t)))

      (match t
        [(or 'Boolean 'Real 'Char 'String 'Bytes 'Keyword 'Tvar) #t]
        [`(Values ,ts ...)
         (andmap no-occur-tv? ts)]
        [`[-> ,I ,O : #:+ ,T #:- ,F]
         (and (no-occur-tv? I) (no-occur-tv? O))]
        [(? tvar?) (not (eq? tv t))])))

  (: unifier! [-> Type Type Subst Exp Void])
  (define unifier!
    (λ (t1 t2 s exp)
      (let ([t1 (if (tvar? t1) (apply-subst-to-type t1 s) t1)]
            [t2 (if (tvar? t2) (apply-subst-to-type t2 s) t2)])
        (match* (t1 t2)
          [(_ _) #:when (equal? t1 t2) (void)]
          [((? tvar?) _) (safe-extend-subst! s t1 t2 exp)]
          [(_ (? tvar?)) (safe-extend-subst! s t2 t1 exp)]
          [(`(Values ,ts1 ...)
            `(Values ,ts2 ...))
           (for ([t1 (in-list ts1)]
                 [t2 (in-list ts2)])
             (unifier! t1 t2 s exp))]
          [(`[-> ,I1 ,O1 : #:+ ,T1 #:- ,F1]
            `[-> ,I2 ,O2 : #:+ ,T2 #:- ,F2])
           (unifier! I1 I2 s exp)
           (unifier! O1 O2 s exp)]))))


  (: apply-one-subst [-> Type Tvar Type Type])
  (define apply-one-subst
    (λ (t0 tv t1)
      (: apply-t [-> Type Type])
      (define apply-t (λ (t0) (apply-one-subst t0 tv t1)))

      (match t0
        [(or 'Boolean 'Real 'Char 'String 'Bytes 'Keyword 'Tvar) t0]
        [`(Values ,ts ...)
         `(Values ,@(map apply-t ts))]
        [`[-> ,I ,O : #:+ ,T #:- ,F]
         `[-> ,(apply-t I) ,(apply-t O) : #:+ ,T #:- ,F]]
        [(? tvar?) (if (eq? t0 tv) t1 t0)])))

  (: apply-subst-to-type [-> Type Subst Type])
  (define apply-subst-to-type
    (λ (t s)
      (: apply-t [-> Type Type])
      (define apply-t (λ (t) (apply-subst-to-type t s)))

      (match t
        [(or 'Boolean 'Real 'Char 'String 'Bytes 'Keyword 'Tvar) t]
        [`(Values ,ts ...)
         `(Values ,@(map apply-t ts))]
        [`[-> ,I ,O : #:+ ,T #:- ,F]
         `[-> ,(apply-t I) ,(apply-t O) : #:+ ,T #:- ,F]]
        [(? tvar?)
         (define r (hash-ref s t #f))
         (when r (extend-subst! s t (apply-subst-to-type r s)))
         (or (hash-ref s t #f) t)])))

  )
