#lang typed/racket

(require "../types/types.rkt")

(provide subst^)


(define-signature subst^
  (
   [empty-subst        : [-> Subst]]
   [extend-subst!      : [-> Subst Tvar Type Void]]
   [safe-extend-subst! : [-> Subst Tvar Type Exp Void]]

   [no-occurrence? : [-> Tvar Type Boolean]]
   [unifier!       : [-> Type Type Subst Exp Void]]

   [apply-one-subst     : [-> Type Tvar Type Type]]
   [apply-subst-to-type : [-> Type Subst Type]]
   ))
