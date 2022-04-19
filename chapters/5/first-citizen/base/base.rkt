#lang typed/racket

(require "../types/types.rkt"
         "../Parse/parse.rkt"
         "../Continuation/cont-sig.rkt"
         "../Continuation/cont-unit.rkt"
         "../Thread/thd-sig.rkt"
         "../Thread/thd-unit.rkt"
         "../Scheduler/sche-sig.rkt"
         "../Scheduler/sche-unit.rkt"
         "../Mutex/mut-sig.rkt"
         "../Mutex/mut-unit.rkt"
         "../ExpValues/values-sig.rkt"
         "../ExpValues/values-unit.rkt"
         "../Procedure/proc-sig.rkt"
         "../Procedure/proc-unit.rkt"
         "../Environment/env-sig.rkt"
         "../Environment/env-unit.rkt"
         "../Expressions/exp-sig.rkt"
         "../Expressions/exp-unit.rkt")

(provide (all-from-out "../types/types.rkt")
         (all-from-out "../Parse/parse.rkt")
         (all-defined-out))


(define-compound-unit/infer base@
  (import)
  (export cont^ thd^ sche^ mut^ values^ env^ proc^ exp^)
  (link   cont@ thd@ sche@ mut@ values@ env@ proc@ exp@))

(define-values/invoke-unit base@
  (import)
  (export cont^ thd^ sche^ mut^ values^ env^ proc^ exp^))


(define-namespace-anchor ns-anchor)
(define eval-ns (namespace-anchor->namespace ns-anchor))

(: *eval* [->* (S-Exp Env Cont) (Exact-Positive-Integer) ExpVal])
(define *eval*
  (λ (code env cont [timeslice 1])
    (: exp Exp)
    (define exp
      (assert (call-with-values
               (λ () (eval (parse code) eval-ns))
               (λ args (car args)))
              exp?))


    ;; (pretty-print code)
    (initialize-scheduler! timeslice)
    (initialize-thread-identifier!)
    (value-of/k exp env cont)))


(let ()
  (: nullary-func [-> [-> Any] [-> DenVal * ExpVal]])
  (define nullary-func
    (λ (func)
      (λ vals
        (match vals
          ['() (s-expval->expval (func))]
          [_ (error 'nullary-func "Bad args: ~s" vals)]))))


  (: unary-arithmetic-pred [-> [-> Real Boolean] [-> DenVal * ExpVal]])
  (define unary-arithmetic-pred
    (λ (pred)
      (λ vals
        (match vals
          [`(,val) (bool-val (pred (expval->num val)))]
          [_ (error 'unary-pred "Bad args: ~s" vals)]))))

  (: unary-arithmetic-func [-> [-> Real Real] [-> DenVal * ExpVal]])
  (define unary-arithmetic-func
    (λ (func)
      (λ vals
        (match vals
          [`(,val) (num-val (func (expval->num val)))]
          [_ (error 'unary-func "Bad args: ~s" vals)]))))

  (: unary-func [-> [-> Any Void] [-> DenVal * ExpVal]])
  (define unary-func
    (λ (func)
      (λ vals
        (match vals
          [`(,val) (s-expval->expval (func val))]
          [_ (error 'unary-func "Bad args: ~s" vals)]))))


  (: binary-equal-relation [-> [-> Any Any Boolean] [-> DenVal * ExpVal]])
  (define binary-equal-relation
    (λ (relation)
      (λ vals
        (match vals
          [`(,val-1 ,val-2)
           (bool-val (relation val-1 val-2))]
          [_ (error 'binary-relation "Bad args: ~s" vals)]))))


  (: binary-arithmetic-relation [-> [-> Real Real Boolean] [-> DenVal * ExpVal]])
  (define binary-arithmetic-relation
    (λ (relation)
      (λ vals
        (match vals
          [`(,val-1 ,val-2)
           (bool-val (relation (expval->num val-1)
                               (expval->num val-2)))]
          [_ (error 'binary-relation "Bad args: ~s" vals)]))))


  (: n-ary-arithmetic-func [-> [-> Real Real * Real] [-> DenVal * ExpVal]])
  (define n-ary-arithmetic-func
    (λ (func)
      (λ vals
        (match vals
          [`(,val-1 . ,(? list? vals))
           (num-val (apply func
                           (expval->num val-1)
                           (map (λ ([val : DenVal]) : Real
                                  (expval->num val))
                                vals)))]
          [_ (error 'n-ary-arithmetic-func "Bad args: ~s" vals)]))))

  (: n-ary-logic-func [-> [-> Boolean * Boolean] [-> DenVal * ExpVal]])
  (define n-ary-logic-func
    (λ (func)
      (λ vals
        (bool-val (apply func
                         (map (λ ([val : DenVal]) : Boolean
                                (expval->bool val))
                              vals))))))


  (: add-primitive-proc! [-> Symbol [-> DenVal * ExpVal] Void])
  (define add-primitive-proc!
    (λ (op-name op-val)
      (add-denval! op-name (primitive-proc op-val))))

  (: add-denval! [-> Symbol DenVal Void])
  (define add-denval!
    (λ (name val)
      #;(displayln name)
      (if (has-binding? (base-env) name)
          (set-binding! (base-env) name val)
          (base-env (extend-env name val (base-env))))))


  (add-denval! 'undefined undefined)
  (add-denval! 'null      null)
  (add-denval! 'empty     empty)
  (add-denval! 'true      true)
  (add-denval! 'false     false)


  (add-primitive-proc! 'get-tid  (nullary-func get-tid))
  (add-primitive-proc! 'get-ptid (nullary-func get-ptid))


  (add-primitive-proc! 'empty-list (λ [vals : DenVal *] : ExpVal '()))


  (add-primitive-proc! 'zero? (unary-arithmetic-pred zero?))
  (add-primitive-proc! 'sub1 (unary-arithmetic-func sub1))
  (add-primitive-proc! 'add1 (unary-arithmetic-func add1))
  (add-primitive-proc! 'not (λ [vals : DenVal *] : ExpVal
                              (match vals
                                [`(,val) (bool-val (not (expval->bool val)))]
                                [_ (error 'unary-func "Bad args: ~s" vals)])))
  (add-primitive-proc! 'car (λ [vals : DenVal *] : ExpVal
                              (match vals
                                [`(,val) (car (expval->pair val))]
                                [_ (error 'unary-func "Bad args: ~s" vals)])))
  (add-primitive-proc! 'cdr (λ [vals : DenVal *] : ExpVal
                              (match vals
                                [`(,val) (cdr (expval->pair val))]
                                [_ (error 'unary-func "Bad args: ~s" vals)])))
  (let ()
    (: get-op [-> String Symbol])
    (define get-op (λ (ad*) (string->symbol (string-append "c" ad* "r"))))

    (void
     (for/fold ([prevs : (Listof String) '("a" "d")])
               ([i (in-range 1 4)])
       (for*/list : (Listof String)
                  ([curr (in-list '("a" "d"))]
                   [prev (in-list prevs)])
         (define now (string-append curr prev))
         (add-denval! (get-op now)
                      (expval->denval
                       (*eval* `(λ (arg)
                                  (,(get-op curr)
                                   (,(get-op prev)
                                    arg)))
                               (base-env)
                               (id-cont))))
         now))))

  (add-primitive-proc! 'null? (λ [vals : DenVal *] : ExpVal
                                (match vals
                                  [`(,val) (bool-val (null? val))]
                                  [_ (error 'unary-func "Bad args: ~s" vals)])))


  (add-primitive-proc! 'read    (nullary-func read))
  (add-primitive-proc! 'newline (nullary-func newline))

  (add-primitive-proc! 'display (unary-func display))
  (add-primitive-proc! 'print   (unary-func print))
  (add-primitive-proc! 'write   (unary-func write))

  (add-primitive-proc! 'displayln (unary-func displayln))
  (add-primitive-proc! 'println   (unary-func println))
  (add-primitive-proc! 'writeln   (unary-func writeln))

  (add-primitive-proc! 'pretty-display (unary-func pretty-display))
  (add-primitive-proc! 'pretty-print   (unary-func pretty-print))
  (add-primitive-proc! 'pretty-write   (unary-func pretty-write))


  (add-primitive-proc! '=  (binary-arithmetic-relation =))
  (add-primitive-proc! '>  (binary-arithmetic-relation >))
  (add-primitive-proc! '>= (binary-arithmetic-relation >=))
  (add-primitive-proc! '<  (binary-arithmetic-relation <))
  (add-primitive-proc! '<= (binary-arithmetic-relation <=))

  (add-primitive-proc! 'eq?    (binary-equal-relation eq?))
  (add-primitive-proc! 'eqv?   (binary-equal-relation eqv?))
  (add-primitive-proc! 'equal? (binary-equal-relation equal?))


  (add-primitive-proc! 'cons (λ [vals : DenVal *] : ExpVal
                               (match vals
                                 [`(,val-1 ,val-2) (pair-val (cons val-1 val-2))]
                                 [_ (error 'binary-func "Bad args: ~s" vals)])))


  (add-primitive-proc! '+ (n-ary-arithmetic-func +))
  (add-primitive-proc! '- (n-ary-arithmetic-func -))
  (add-primitive-proc! '* (n-ary-arithmetic-func *))
  (add-primitive-proc! '/ (n-ary-arithmetic-func /))

  (add-primitive-proc! 'list (λ [vals : DenVal *] : ExpVal (list-val vals)))
  (add-primitive-proc! 'format (λ [vals : DenVal *] : ExpVal
                                 (match vals
                                   [`(,str ,args ...)
                                    (if (string? str)
                                        (string-val (apply format str args))
                                        (error 'format "Bad arg: ~s" str))]
                                   [_ (error 'n-ary-func "Bad args: ~s" vals)])))


  (add-denval! 'apply
               (proc-val (procedure '(func args)
                                    (call-exp (var-exp 'func) (var-exp 'args))
                                    (empty-env))))

  (add-denval! 'memo-λ
               (expval->denval
                (*eval* '(λ (f)
                           (let ([already-run? #f] [result #f])
                             (λ ()
                               (unless already-run?
                                 (set! result (f))
                                 (set! already-run? #t))
                               result)))
                        (base-env)
                        (id-cont))))

  (add-denval! 'force
               (expval->denval
                (*eval* '(λ (thk) (thk))
                        (base-env)
                        (id-cont))))

  (add-denval! 'void
               (expval->denval
                (*eval* '(λ _ (cond [#f _]))
                        (base-env)
                        (id-cont))))

  (add-denval! 'Y
               (expval->denval
                (*eval* '(λ (f)
                           ((λ (recur-func)
                              (recur-func recur-func))
                            (λ (recur-func)
                              (f (λ args
                                   (apply (recur-func recur-func) args))))))
                        (base-env)
                        (id-cont))))


  (add-denval! 'map
               (expval->denval
                (*eval* '(Y (λ (map)
                              (λ (func ls)
                                (if (null? ls)
                                    '()
                                    (cons (func (car ls))
                                          (map func (cdr ls)))))))
                        (base-env)
                        (id-cont))))

  (add-denval! 'Y*
               (expval->denval
                (*eval* '(λ funcs
                           ((λ (recur-funcs)
                              (recur-funcs recur-funcs))
                            (λ (recur-funcs)
                              (map (λ (func)
                                     (λ args
                                       (apply (apply func (recur-funcs recur-funcs)) args)))
                                   funcs))))
                        (base-env)
                        (id-cont))))

  (add-denval! '*amb*
               (expval->denval
                (*eval* '(λ () (raise "amb: Amb tree exhausted!"))
                        (base-env)
                        (id-cont))))

  (add-denval! 'call/cc
               (expval->denval
                (*eval* '(λ (cont) (let/cc cc (cont cc)))
                        (base-env)
                        (id-cont))))


  (void))
