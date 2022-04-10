#lang typed/racket/base

(require "../types/types.rkt"
         "parser.rkt")

(provide parse
         (all-from-out "parser.rkt"))


(: parse [-> S-Exp S-Exp])
(define parse
  (λ (code)
    (parser code)))
