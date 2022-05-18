#lang typed/racket

(require "../types/types.rkt"
         "cont-sig.rkt")

(provide cont@)


(define-unit cont@
  (import)
  (export cont^)

  (: id-cont [-> Null])
  (define id-cont
    (let ([cont '()])
      (λ () cont)))

  (: end-cont [-> Null])
  (define end-cont
    (let ([cont '()])
      (λ ()
        #;(displayln "End Program!")
        cont)))

  (: apply-cont [-> Cont ExpVal FinalAnswer])
  (define apply-cont
    (λ (cont val)
      (if (null? cont)
          (final-answer val)
          (((frame-func (car cont)) (cdr cont)) val))))

  )
