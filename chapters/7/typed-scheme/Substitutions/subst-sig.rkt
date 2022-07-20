#lang typed/racket

(require "../types/types.rkt")

(provide subst^)


(define-signature subst^
  (
   [empty-subst       : [-> Subst]]
   [extend-subst      : [-> Subst Tvar Type Subst]]
   [safe-extend-subst : [-> Subst Tvar Type Exp Subst]]

   [no-occurrence? : [-> Tvar Type Boolean]]
   [unifier        : [-> Type Type Subst Exp Subst]]

   [apply-one-subst     : [-> Type Tvar Type Type]]
   [apply-subst-to-type : [-> Type Subst Type]]
   ))
