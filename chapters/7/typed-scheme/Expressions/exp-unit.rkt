#lang typed/racket

(require "../types/types.rkt"
         "../Continuation/cont-sig.rkt"
         "../ExpValues/values-sig.rkt"
         "../Environment/env-sig.rkt"
         "../TypeEnvironment/tenv-sig.rkt"
         "../RelationEnvironment/renv-sig.rkt"
         "../Procedure/proc-sig.rkt"
         "exp-sig.rkt")

(provide exp@)


(: raise-type-of-error [-> Any Any Any Nothing])
(define raise-type-of-error
  (λ (expected given in)
    (raise-arguments-error
     'type-of   "type mismatch"
     "expected" expected
     "given"    given
     "in"       in)))


(define-unit exp@
  (import cont^ values^ env^ tenv^ renv^ proc^)
  (export exp^)


  (: value-of/k [-> Exp Env Cont FinalAnswer])
  (define value-of/k
    (λ (exp env cont)
      (match exp
        [(ann-exp  exp type)  (value-of/k exp env cont)]
        [(cast-exp exp type)  (value-of/k exp env cont)]
        [(inst-exp exp types) (value-of/k exp env cont)]

        [(assign-exp var exp)
         (value-of/k
          exp env
          (cons
           (frame
            'assign-frame
            (ann (λ (cont)
                   (λ (val)
                     (apply-cont cont (set-binding! env var (expval->denval val)))))
                 [-> Cont [-> ExpVal FinalAnswer]]))
           cont))]
        [(quote-exp  datum) (apply-cont cont (s-expval->denval datum))]
        [(symbol-exp sym)   (apply-cont cont (symbol-val sym))]
        [(real-exp   num)   (apply-cont cont (num-val    num))]
        [(bool-exp   bool)  (apply-cont cont (bool-val   bool))]
        [(char-exp   char)  (apply-cont cont (char-val   char))]
        [(string-exp str)   (apply-cont cont (string-val str))]
        [(bytes-exp  bs)    (apply-cont cont (bytes-val  bs))]

        [(var-exp    var)   (apply-cont cont (apply-env env var))]

        [(begin-exp exps)
         (value-of/k
          (car exps) env
          (cons
           (frame
            'begin-frame
            (ann (λ (cont)
                   (λ (val)
                     (let ([exps (cdr exps)])
                       (if (null? exps)
                           (apply-cont cont val)
                           (value-of/k (begin-exp exps) env cont)))))
                 [-> Cont [-> ExpVal FinalAnswer]]))
           cont))]

        [(if-exp pred-exp true-exp false-exp)
         (value-of/k
          pred-exp env
          (cons
           (frame
            'if-frame
            (ann (λ (cont)
                   (λ (pred-val)
                     (value-of/k
                      (if (expval->bool pred-val)
                          true-exp
                          false-exp)
                      env cont)))
                 [-> Cont [-> ExpVal FinalAnswer]]))
           cont))]

        [(new-closure-exp exp)
         (value-of/k
          exp env
          (cons
           (frame
            'new-closure-frame
            (ann (λ (cont)
                   (λ (op)
                     (cond [(proc? op)
                            (apply-cont cont
                                        (if (thread-share-memory?)
                                            op
                                            (proc (proc-vars op)
                                                  (proc-body op)
                                                  (copy-env (proc-saved-env op)))))]
                           [(cont? op) (apply-cont cont op)]
                           [(primitive-proc? op) (apply-cont cont op)]
                           [else (raise-argument-error 'value-of/k "operator?" op)])))
                 [-> Cont [-> ExpVal FinalAnswer]]))
           cont))]
        [(trace-proc-exp vars body)
         (apply-cont cont (proc-val (trace-procedure vars body env)))]
        [(proc-exp vars body)
         (apply-cont cont (proc-val (procedure vars body env)))]
        [(call-exp rator rands)
         (value-of/k
          rator env
          (cons
           (frame
            'call-rator-frame
            (ann (λ (cont)
                   (λ (op)
                     (unless (or (proc? op) (cont? op) (primitive-proc? op))
                       (raise-argument-error 'value-of/k "operator?" op))

                     (if (var-exp? rands)
                         (value-of/k
                          rands env
                          (cons
                           (frame
                            'call-rator-frame
                            (ann (λ (cont)
                                   (λ (args)
                                     (cond [(proc? op)
                                            (apply-procedure/k op (expval->list args) cont)]
                                           [(cont? op)
                                            (apply-cont op (car (expval->list args)))]
                                           [(primitive-proc? op)
                                            (apply-cont cont (apply (primitive-proc-λ op) (expval->list args)))])))
                                 [-> Cont [-> ExpVal FinalAnswer]]))
                           cont))
                         (let loop : FinalAnswer
                              ([rands rands] [args : (Listof DenVal) '()])
                           (if (null? rands)
                               (cond [(proc? op)
                                      (apply-procedure/k op (reverse args) cont)]
                                     [(cont? op)
                                      (apply-cont op (car (last-pair args)))]
                                     [(primitive-proc? op)
                                      (apply-cont cont (apply (primitive-proc-λ op) (reverse args)))])
                               (value-of/k
                                (car rands) env
                                (cons
                                 (frame
                                  'call-rator-frame
                                  (ann (λ (cont)
                                         (λ (arg)
                                           (loop (cdr rands)
                                                 (cons (expval->denval arg) args))))
                                       [-> Cont [-> ExpVal FinalAnswer]]))
                                 cont)))))))
                 [-> Cont [-> ExpVal FinalAnswer]]))
           cont))])))

  (: type-of [-> Exp TEnv REnv (Option Type) Type])
  (define type-of
    (let ()
      (: parse-poly [-> Type (Values (Listof Type) Type)])
      (define parse-poly
        (λ (ptype)
          (let loop : (Values (Listof Type) Type)
               ([tvars : (Listof Type) '()]
                [R ptype])
            (match R
              [`(All (,A) ,T)
               #:when (and (type? A) (type? T))
               (loop (cons A tvars) T)]
              [_ (values (reverse tvars) R)]))))

      (: inst-type [-> Type Type Type * Type])
      (define inst-type
        (λ (t0 t . ts)
          (define-values (tv tb) (parse-poly t0))
          (assert (> (length tv) (length ts)))

          (: renv (Immutable-HashTable Type Type))
          (define renv
            (for/hash : (Immutable-HashTable Type Type)
                      ([k (in-list tv)]
                       [v (in-list (cons t ts))])
              (values k v)))

          (let loop : Type ([T tb])
            (match T
              [B #:when (hash-has-key? renv B) (hash-ref renv B)]
              [(? list?) (map loop T)]
              [_ T]))))


      (λ (exp tenv renv t0)
        (: check [-> Type Type])
        (define check
          (λ (t1)
            (if (or (false? t0) (<=: t1 t0))
                (match t1
                  [`(Values ,t) t]
                  [_ t1])
                (raise-type-of-error t0 t1 exp))))

        (match exp
          [(ann-exp  exp type)  (begin0 (check (apply-renv renv type (λ () type))) (type-of exp tenv renv type))]
          [(cast-exp exp type)  (begin0 (apply-renv renv type (λ () type)) (type-of exp tenv renv 'Any))]
          [(inst-exp exp types) (check (apply inst-type (type-of exp tenv renv #f) types))]

          [(assign-exp var exp) (begin0 (check 'Void) (type-of exp tenv renv (apply-tenv tenv var)))]

          [(symbol-exp sym)  (check 'Symbol)]
          [(real-exp   num)  (check 'Real)]
          [(bool-exp   #t)   (check 'True)]
          [(bool-exp   #f)   (check 'False)]
          [(char-exp   char) (check 'Char)]
          [(string-exp str)  (check 'String)]

          [(var-exp var)     (check (apply-tenv tenv var))]

          [(begin-exp (cons exp exps))
           (cond [(null? exps) (type-of exp tenv renv t0)]
                 [else
                  (type-of exp tenv renv #f)
                  (type-of (begin-exp exps) tenv renv t0)])]

          [(if-exp pred-exp true-exp false-exp)
           (define tp (type-of pred-exp  tenv renv 'Boolean))
           (define tt (type-of true-exp  tenv renv t0))
           (define tf (type-of false-exp tenv renv t0))
           (case tp
             [(True)    tt]
             [(False)   tf]
             [(Boolean) (check (type-union tt tf))]
             [else (raise-type-of-error 'Boolean tp pred-exp)])]


          #;[(trace-proc-exp vars body) (type-of (proc-exp vars body) tenv renv t0)]
          [(proc-exp vars body)
           (if t0
               (begin0 t0
                 (match t0
                   [`(All (,A) ,T)
                    #:when (and (type? A) (type? T))
                    (type-of exp tenv renv T)]
                   #;[`(All (,A ,..) ,T)
                      #:when (and (type? A)
                                  (eq? .. '...)
                                  (type? T))
                      (type-of exp tenv renv T)]
                   [`[-> ,I ,O : #:+ ,T #:- ,F]
                    #:when (and (type? I) (type? O)
                                (type? T) (type? F))
                    (match I
                      [`(Values ,ts ... ,t* *)
                       #:when (and (symbol? vars)
                                   ((listof? type?) ts)
                                   (type? t*))
                       (type-of body (extend-tenv vars (desugar-type `(List* ,@ts (Listof ,t*))) tenv) renv O)]
                      [`(Values ,ts ...)
                       #:when ((listof? type?) ts)
                       (type-of body
                                (if (list? vars)
                                    (extend-tenv* vars ts tenv)
                                    (extend-tenv  vars (desugar-type `(List ,@ts)) tenv))
                                renv
                                O)])]))
               (desugar-type
                (cond
                  [(list? vars)
                   (: ts (Listof 'Any))
                   (define ts (make-list (length vars) 'Any))
                   `[-> (Values ,@ts) ,(type-of body (extend-tenv* vars ts tenv) renv #f)]]
                  [else `[-> (Values Any *) ,(type-of body (extend-tenv vars '(Listof Any) tenv) renv #f)]])))]
          [(call-exp rator rands)
           (: ts0 (Listof Type))
           (define ts0
             (if (list? rands)
                 (for/list : (Listof Type)
                           ([rand (in-list rands)])
                   (type-of rand tenv renv #f))
                 (let loop : (Listof Type)
                      ([t (type-of rands tenv renv #f)]
                       [res : (Listof Type) '()])
                   (match t
                     ['Null (reverse res)]
                     [`(Pair ,A ,B)
                      (loop B (cons A res))]))))

           (match rator
             [(proc-exp vars body)
              (type-of body
                       (if (list? vars)
                           (extend-tenv* vars ts0 tenv)
                           (extend-tenv  vars (desugar-type `(List ,@ts0)) tenv))
                       renv
                       t0)]
             [_
              (define t (type-of rator tenv renv #f))
              (define T
                (match/values (parse-poly t)
                  [('() _) t]
                  [(tvars `[-> ,I ,O : #:+ ,T #:- ,F])
                   #:when (and (type? I) (type? O)
                               (type? T) (type? F))
                   (: menv (Mutable-HashTable Type (Option Type)))
                   (define menv (make-hash))
                   (for ([tvar (in-list tvars)]) (hash-set! menv tvar #f))
                   (let ([O  : Type (desugar-type O)]
                         [s0 : Type
                             (match I
                               [`(Values ,ts ... ,t* *)
                                #:when (and ((listof? type?) ts)
                                            (type? t*))
                                `(Values ,@ts0 *)]
                               [`(Values ,ts ...)
                                #:when (and ((listof? type?) ts)
                                            (= (length ts0) (length ts)))
                                `(Values ,@ts0)])])
                     (: match-types! [-> Type Type Void])
                     (define match-types!
                       (λ (formal actual)
                         (match* (formal actual)
                           [((? keyword?) (? keyword?)) (void)]
                           [((? symbol?) _)
                            (cond
                              [(hash-has-key? menv formal)
                               (define t (hash-ref menv formal))
                               (or (and t (unless (=: t actual) (raise-type-of-error t actual exp)))
                                   (hash-set! menv formal actual))]
                              [(=: formal actual) (void)]
                              [else (raise-type-of-error formal actual exp)])]
                           [((? list?) (? list?))
                            #:when (= (length formal)
                                      (length actual))
                            (for-each match-types! formal actual)])))

                     (match-types! I s0)
                     (and t0 (match-types! O t0))
                     (apply inst-type t
                            (assert
                             (for/list : (Listof Type) ([tvar (in-list tvars)])
                               (assert (hash-ref menv tvar)))
                             pair?)))]))

              (match T
                [`[-> ,I ,O : #:+ ,T #:- ,F]
                 #:when (and (type? I) (type? O)
                             (type? T) (type? F))
                 (begin0 (check O)
                   (match I
                     [`(Values ,ts ... ,t* *)
                      #:when (and ((listof? type?) ts)
                                  (type? t*))
                      (for ([t0 (in-list ts0)]
                            [t  (in-list (append ts (make-list (- (length ts0) (length ts)) t*)))])
                        (unless (<=: t0 t)
                          (raise-type-of-error t0 t exp)))]
                     [`(Values ,ts ...)
                      #:when (and ((listof? type?) ts)
                                  (= (length ts0) (length ts)))
                      (for ([t0 (in-list ts0)]
                            [t  (in-list ts)])
                        (unless (<=: t0 t)
                          (raise-type-of-error t0 t exp)))]))])])]))))

  )
