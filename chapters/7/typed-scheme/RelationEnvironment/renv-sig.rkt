#lang typed/racket

(require "../types/types.rkt")

(provide renv^)


(define-signature renv^
  (
   [base-renv    : (Parameter REnv)]
   [empty-renv   : [-> REnv]]
   [empty-renv?  : [-> REnv Boolean]]


   [extend-renv  : [-> Symbol Type REnv REnv]]
   [extend-renv* : [-> (Listof Symbol) (Listof Type) REnv REnv]]
   [extend-renv+ : [-> (Listof (Pair Symbol Type)) REnv REnv]]

   [extend-renv? : [-> REnv Boolean]]

   [renv?          : [-> Any Boolean : REnv]]
   [apply-renv     : [->* (REnv Type) ([-> Type]) Type]]
   [has-rbinding?  : [-> REnv Symbol Boolean]]
   ))
