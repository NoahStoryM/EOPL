#lang typed/racket

(require "../base/base.rkt"
         "../Parse/parser.rkt")

(provide *repl*)


(: *repl* [-> Env Void])
(define *repl*
  (λ (env)
    (display "]=> ")
    (let ([code : S-Exp (cast (read) S-Exp)])
      (case code
        ['(exit) (void)]
        [else
         (writeln (*eval* code env))
         (*repl* env)]))))


;; (*repl* (base-env))
