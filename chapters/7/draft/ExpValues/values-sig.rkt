#lang typed/racket

(require "../types/types.rkt")

(provide values^)


(define-signature values^
  (
   [symbol-val         : [-> Symbol DenVal]]
   [num-val            : [-> Real DenVal]]
   [bool-val           : [-> Boolean DenVal]]
   [char-val           : [-> Char DenVal]]
   [string-val         : [-> String DenVal]]
   [pair-val           : [-> (Pair DenVal DenVal) DenVal]]
   [list-val           : [-> (Listof DenVal) DenVal]]
   [proc-val           : [-> Proc DenVal]]
   [primitive-proc-val : [-> Primitive-Proc DenVal]]
   [mutex-val          : [-> Mutex DenVal]]

   [expval->symbol         : [-> ExpVal Symbol]]
   [expval->num            : [-> ExpVal Real]]
   [expval->bool           : [-> ExpVal Boolean]]
   [expval->char           : [-> ExpVal Char]]
   [expval->string         : [-> ExpVal String]]
   [expval->pair           : [-> ExpVal (Pair DenVal DenVal)]]
   [expval->list           : [-> ExpVal (Listof DenVal)]]
   [expval->proc           : [-> ExpVal Proc]]
   [expval->primitive-proc : [-> ExpVal Primitive-Proc]]
   [expval->mutex          : [-> ExpVal Mutex]]

   [expval->denval   : [-> ExpVal DenVal]]
   [s-expval->expval : [-> Any ExpVal]]
   ))
