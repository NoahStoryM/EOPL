#lang typed/racket/base

(require "../types/types.rkt"
         "desugar.rkt"
         "auto-cps.rkt"
         "parser.rkt")

(provide parse
         parser
         auto-cps
         desugar)


(: parse [-> S-Exp S-Exp])
(define parse
  (λ (code)
    (parser
     (desugar
      (auto-cps
       (desugar
        code))))))
