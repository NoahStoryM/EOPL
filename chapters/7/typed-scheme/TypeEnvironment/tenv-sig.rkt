#lang typed/racket

(require "../types/types.rkt")

(provide tenv^)


(define-signature tenv^
  (
   [base-tenv    : (Parameter TEnv)]
   [empty-tenv   : [-> TEnv]]
   [empty-tenv?  : [-> TEnv Boolean]]


   [extend-tenv  : [-> Tvar Type TEnv TEnv]]
   [extend-tenv* : [-> (Listof Tvar) (Listof Type) TEnv TEnv]]
   [extend-tenv+ : [-> (Listof (Pair Tvar Type)) TEnv TEnv]]

   [extend-tenv? : [-> TEnv Boolean]]

   [tenv?          : [-> Any Boolean : TEnv]]
   [apply-tenv     : [->* (TEnv Tvar) ([-> Type]) Type]]
   [has-tbinding?  : [-> TEnv Tvar Boolean]]
   ))
