#lang typed/racket

(require "../types/types.rkt")

(provide subst^)


(define-signature subst^
  (
   [empty-subst  : [-> Subst]]
   [extend-subst : [-> Subst Tvar Type Subst]]

   [apply-one-subst     : [-> Type Tvar Type Type]]
   [apply-subst-to-type : [-> Type Subst Type]]
   ))
