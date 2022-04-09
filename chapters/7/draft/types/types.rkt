#lang typed/racket

(require/typed racket/undefined
  [undefined Undefined])

(require typed/racket/unsafe)

(unsafe-require/typed
 racket/contract/base
 [or/c
  (All (A B C D E)
       (case-> [-> (pred Any)]
               [-> (pred A) (pred A)]
               [-> (pred A) (pred B) (pred (U A B))]
               [-> (pred A) (pred B) (pred C) (pred (U A B C))]
               [-> (pred A) (pred B) (pred C) (pred D) (pred (U A B C D))]
               [-> (pred A) (pred B) (pred C) (pred D) (pred E) (pred (U A B C D E))]))]
 [and/c
  (All (A B C D E)
       (case-> [-> (pred Nothing)]
               [-> (pred A) (pred A)]
               [-> (pred A) (pred B) (pred (∩ A B))]
               [-> (pred A) (pred B) (pred C) (pred (∩ A B C))]
               [-> (pred A) (pred B) (pred C) (pred D) (pred (∩ A B C D))]
               [-> (pred A) (pred B) (pred C) (pred D) (pred E) (pred (∩ A B C D E))]))])

(provide undefined or/c and/c (all-defined-out))


(define-predicate undefined? Undefined)
(define-predicate true? True)


(define-type Literal (U Boolean Real Symbol Char String))


(define-type S-Exp  (U Literal S-List))
(define-predicate s-exp?  S-Exp)
(define-type S-List (Listof S-Exp))
(define-predicate s-list? S-List)

(define-type Ann-S-Exp  (U Literal (List 'ann S-Exp Type)))
(define-predicate ann-s-exp?  Ann-S-Exp)


(define-type Lambda (U 'lambda 'λ))
(define-predicate λ? Lambda)
(define-predicate lambda? Lambda)

(define-type Trace-Lambda (U 'trace-lambda 'trace-λ))
(define-predicate trace-λ? Trace-Lambda)
(define-predicate trace-lambda? Trace-Lambda)


(define-type DenVal (U Literal Undefined
                       Primitive-Proc
                       Proc Trace-Proc
                       Cont Mutex Thread-Identifier
                       Null (Pair DenVal DenVal)))
(define-predicate denval?  DenVal)
(define-predicate denpair? (Pair DenVal DenVal))
(define-predicate denlist? (Listof DenVal))

(define-type ExpVal (U DenVal Void Nothing))
(define-predicate expval? ExpVal)

(define-new-subtype FinalAnswer (final-answer ExpVal))
(define-predicate final-answer? FinalAnswer)


(define-type (Queueof A) (List (Listof A) (Listof A)))
(define-predicate empty-queue? (Queueof Nothing))

(: empty-queue [-> (Queueof Nothing)])
(define empty-queue (let ([empty-q '(() ())]) (λ () empty-q)))

(: enqueue (All (A) [-> (Queueof A) A (Queueof A)]))
(define enqueue
  (λ (q arg)
    (match q
      [`(,in ,out)
       `((,arg . ,in) ,out)])))

(: dequeue (All (A B) [-> (Queueof A) [-> A (Queueof A) B] B]))
(define dequeue
  (λ (q f)
    (match q
      ['(() ())
       (raise-argument-error 'dequeue "non-empty-queue?" q)]
      [`(,in ())
       (define l (reverse in))
       (f (car l) `(() ,(cdr l)))]
      [`(,in (,1st . ,out))
       (f 1st `(,in ,out))])))


(struct thd
  ([ptid : Natural]
   [tid  : Natural]
   [mail : (Boxof (Queueof DenVal))]
   [time-slice : Exact-Positive-Integer]
   [thunk : [-> FinalAnswer]])
  #:type-name Thd)

(struct thread-identifier
  ([tid  : Natural]
   [ptid : Natural])
  #:type-name Thread-Identifier)

(: thread-share-memory? (Parameter Boolean))
(define thread-share-memory? (make-parameter #f))


(define-type Cont (Listof Frame))
(define-predicate cont? Cont)

(define-type Handlers-Cont (Pair Handlers-Frame Cont))
(define-predicate handlers-cont? Handlers-Cont)

(struct frame
  ([type : Symbol]
   [handlers-cont : (Option Handlers-Cont)]
   [func : [-> Cont [-> ExpVal FinalAnswer]]])
  #:type-name Frame)

(struct handlers-frame frame
  ([preds    : (Listof Proc)]
   [handlers : (Listof Proc)])
  #:type-name Handlers-Frame)


(define-struct ref
  ([val : DenVal])
  #:mutable
  #:type-name Ref)

(define-struct env
  ([type  : (U 'empty-env 'extend-env 'extend-env-rec)]
   [binds : (Immutable-HashTable Symbol Ref)])
  #:type-name Env)

(define-struct tenv
  ([type  : (U 'empty-tenv 'extend-tenv 'extend-tenv-rec)]
   [binds : (Immutable-HashTable Symbol (Option Type))])
  #:type-name TEnv)


(define-struct primitive-proc
  ([λ : [-> DenVal * ExpVal]])
  #:type-name Primitive-Proc)

(define-struct proc
  ([vars : (U Symbol (Listof Symbol))]  ; Symbol is used for `apply-primitive'.
   [body : Exp]
   [saved-env : Env])
  #:type-name Proc)

(define-struct (trace-proc proc) () #:type-name Trace-Proc)


(define-struct mutex
  ([keys : Natural]
   [wait-queue : (Queueof Natural)])
  #:mutable
  #:type-name Mutex)


(define-type Type (U Symbol Poly-Type λ-Type Opt-Type Pair-Type List-Type Listof-Type))
(define-predicate type? Type)

(define-type Types (Listof Type))
(define-predicate types? Types)

(define-type Poly-Type (List 'All (Listof Symbol) Type))
(define-predicate poly-type? Poly-Type)

(define-type λ-Type (List* '-> Type (Listof Type) #;(Rec End (U (List Type) (List Type '* Type) (Pair Type End)))))
(define-predicate λ-type? λ-Type)

(define-type Opt-Type (List 'Option Type))
(define-predicate opt-type? Opt-Type)

(define-type Pair-Type (List 'Pair Type Type))
(define-predicate pair-type? Pair-Type)

(define-type List-Type (Pair 'List Types))
(define-predicate list-type? List-Type)

(define-type Listof-Type (Pair 'Listof Types))
(define-predicate listof-type? Listof-Type)

(define-predicate any-type?     'Any)
(define-predicate nothing-type? 'Nothing)

(define-predicate void-type?    'Void)
(define-predicate null-type?    'Null)
(define-predicate symbol-type?  'Symbol)
(define-predicate natural-type? 'Natural)
(define-predicate real-type?    (U 'Natural 'Real))
(define-predicate number-type?  (U 'Number 'Natural 'Real))
(define-predicate true-type?    'True)
(define-predicate false-type?   'False)
(define-predicate bool-type?    (U 'True 'False 'Boolean))
(define-predicate char-type?    'Char)
(define-predicate string-type?  'String)

(define-predicate mutex-type?   'Mutex)

(: listof? (All (A) [-> (pred A) (pred (Listof A))]))
(define listof?
  (λ (pred)
    (λ (arg)
      (and (list? arg)
           (andmap pred arg)))))


#;(: λ-args-type [-> λ-Type (U (Listof Type) (Pair (Listof Type) Type))])
#;(define λ-args-type
    (λ (λ-type)
      (match λ-type
        [`[-> ,ts ... ,t1]
         #:when (and (types? ts) (type? t1))
         ts]
        [`[-> ,ts ... ,t* * ,t1]
         #:when (and (types? ts) (type? t*) (type? t1))
         (list ts t*)])))

(: λ-return-type [-> λ-Type Type])
(define λ-return-type
  (λ (λ-type)
    (match λ-type
      [`[-> ,ts ... ,t* * ,t1]
       #:when (and (types? ts) (type? t*) (type? t1))
       t1]
      [`[-> ,ts ... ,t1]
       #:when (and (types? ts) (type? t1))
       t1])))

(: type-union
   (case-> [-> 'Nothing]

           [-> False False]
           [-> Type Type]

           [-> False (Option Type) False]
           [-> (Option Type) False False]
           [-> Type Type Type]

           [-> False (Option Type) (Option Type) (Option Type) * False]
           [-> (Option Type) False (Option Type) (Option Type) * False]
           [-> Type Type Type Type * Type]
           [-> Type Type (Option Type) (Option Type) * (Option Type)]))
(define type-union
  (case-lambda
    [()  'Nothing]
    [(t) t]
    [(t1 t2)
     (and t1 t2
          (or (and (=: t1 t2) t1)
              (and (any-type? t1) t1)
              (and (any-type? t2) t2)
              (and (nothing-type? t1) t2)
              (and (nothing-type? t2) t1)

              (and (true-type?  t1) (false-type? t2) 'Boolean)
              (and (false-type? t1) (true-type?  t2) 'Boolean)

              (and (natural-type? t1) (real-type? t2) 'Real)
              (and (real-type? t1) (natural-type? t2) 'Real)

              'Any))]
    [(t1 t2 . ts)
     (and t1 t2 (types? ts)
          (for/fold ([res : Type (type-union t1 t2)])
                    ([t (in-list ts)])
            (type-union res t)))]))


(: =: [-> Type Type Boolean])
(define =: (λ (t1 t2) (equal? t1 t2)))

(: <=: [-> Type Type Boolean])
(define <=: (λ (t1 t2) (or (=: t1 t2) (<: t1 t2))))

(: >=: [-> Type Type Boolean])
(define >=: (λ (t1 t2) (or (=: t1 t2) (>: t1 t2))))

(: <: [-> Type Type Boolean])           ; TODO
(define <: (const #f))

(: >: [-> Type Type Boolean])           ; TODO
(define >: (const #f))


(define-struct exp () #:transparent #:type-name Exp)


(define-struct (ann-exp exp)
  ([exp  : Exp]
   [type : Type])
  #:transparent
  #:type-name Ann-Exp)


(define-struct (assign-exp exp)
  ([var : Symbol]
   [exp : Exp])
  #:transparent
  #:type-name Assign-Exp)


(define-struct (symbol-exp exp)
  ([symbol : Symbol])
  #:transparent
  #:type-name Symbol-Exp)

(define-struct (real-exp exp)
  ([num : Real])
  #:transparent
  #:type-name Real-Exp)

(define-struct (bool-exp exp)
  ([bool : Boolean])
  #:transparent
  #:type-name Bool-Exp)

(define-struct (char-exp exp)
  ([char : Char])
  #:transparent
  #:type-name Char-Exp)

(define-struct (string-exp exp)
  ([str : String])
  #:transparent
  #:type-name String-Exp)


(define-struct (if-exp exp)
  ([pred-exp  : Exp]
   [true-exp  : Exp]
   [false-exp : Exp])
  #:transparent
  #:type-name If-Exp)


(define-struct (var-exp exp)
  ([var : Symbol])
  #:transparent
  #:type-name Var-Exp)

(define-struct (letrec-exp exp)
  ([bind-vars : (Listof Symbol)]
   [bind-exps : (Listof Exp)]
   [body : Exp])
  #:transparent
  #:type-name Letrec-Exp)


(define-struct (let/cc-exp exp)
  ([cc-var : Symbol]
   [body : Exp])
  #:transparent
  #:type-name Let/CC-Exp)


(define-struct (begin-exp exp)
  ([exps : (Pair Exp (Listof Exp))])
  #:transparent
  #:type-name Begin-Exp)


(define-struct (proc-exp exp)
  ([vars : (U Symbol (Listof Symbol))]
   [body : Exp])
  #:transparent
  #:type-name Proc-Exp)

(define-struct (trace-proc-exp proc-exp)
  ()
  #:transparent
  #:type-name Trace-Proc-Exp)

(define-struct (call-exp exp)
  ([rator : Exp]
   [rands : (U Var-Exp (Listof Exp))])  ; Symbol is used for `apply'.
  #:transparent
  #:type-name Call-Exp)


(define-struct (handlers-exp exp)
  ([catch-preds : (Listof Exp)]
   [catch-bodys : (Listof Exp)]
   [body : Exp])
  #:transparent
  #:type-name Handlers-Exp)

(define-struct (raise-exp exp)
  ([exp : Exp])
  #:transparent
  #:type-name Raise-Exp)


(define-struct (spawn-exp exp)
  ([exp : Exp])
  #:transparent
  #:type-name Spawn-Exp)

(define-struct (mutex-exp exp)
  ([exp : Exp])
  #:transparent
  #:type-name Mutex-Exp)

(define-struct (wait-exp exp)
  ([exp : Exp])
  #:transparent
  #:type-name Wait-Exp)

(define-struct (signal-exp exp)
  ([exp : Exp])
  #:transparent
  #:type-name Signal-Exp)

(define-struct (kill-exp exp)
  ([exp : Exp])
  #:transparent
  #:type-name Kill-Exp)

(define-struct (send-exp exp)
  ([tid-exp   : Exp]
   [value-exp : Exp])
  #:transparent
  #:type-name Send-Exp)

(define-struct (receive-exp exp)
  ()
  #:transparent
  #:type-name Receive-Exp)

(define-struct (try-receive-exp exp)
  ()
  #:transparent
  #:type-name Try-Receive-Exp)

(define-struct (yield-exp exp)
  ()
  #:transparent
  #:type-name Yield-Exp)