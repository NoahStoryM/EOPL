#lang typed/racket

(require "../base/base.rkt")


(define-namespace-anchor ns-anchor)
(define eval-ns (namespace-anchor->namespace ns-anchor))

(: *type-check* [-> S-Exp TEnv (Option Type) Type])
(define *type-check*
  (λ (code tenv t0)
    (: exp Exp)
    (define exp
      (assert (call-with-values
               (λ ()
                 (eval
                  (parser
                   (desugar
                    (auto-ann
                     code)))
                  eval-ns))
               (λ args (car args)))
              exp?))
    (type-of exp tenv t0)))


(displayln "Start type checker test.\n")

(for ([code
       (in-list
        '(
          (begin
            (: dio Boolean)
            (define dio #f)
            (: noah String)
            (define noah "")
            (displayln noah)
            (set! noah "Noah Ma")
            (displayln noah))
          (begin
            (: fact [-> Real Real])
            (define fact
              (λ (n)
                (if (zero? n)
                    1
                    (* n (fact (sub1 n))))))
            (displayln (fact 0))
            (displayln (fact 1))
            (displayln (fact 2))
            (displayln (fact 3)))
          (begin
            (: pdisplayln (All (A) [-> A Void]))
            (define pdisplayln
              (λ (arg)
                (displayln arg)))

            ((inst pdisplayln Real) 123)
            ((inst pdisplayln String) "123")
            ((inst pdisplayln Symbol) '|123|)
            ((inst pdisplayln Any) '123))

          ;;; Type mismatch cases:
          #;(begin
              (: dio Boolean)
              (define dio #f)
              (: noah Symbol)
              (define noah '||)
              (displayln noah)
              (set! noah "Noah Ma")
              (displayln noah))
          #;(begin
              (: pdisplayln (All (A) [-> A Void]))
              (define pdisplayln
                (λ (arg)
                  (displayln arg)))

              ((inst pdisplayln String) 123)
              ((inst pdisplayln String) "123")
              ((inst pdisplayln Symbol) '|123|))
          ))])
  (displayln "----------------------------------------------")
  (pretty-print code)
  (pretty-print (desugar (auto-ann code)))
  (pretty-print (*type-check* code (base-tenv) #f))
  (newline))
