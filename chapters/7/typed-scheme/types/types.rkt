#lang typed/racket


(module types typed/racket
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

  (provide
   undefined
   or/c and/c
   (rename-out
    #;[trace-lambda   trace-λ]
    [case-lambda    case-λ]
    [match-lambda   match-λ]
    [match-lambda*  match-λ*]
    [match-lambda** match-λ**]

    [add1 1+]
    [sub1 1-]
    [sub1 -1+])
   (all-defined-out))


  (define-predicate undefined? Undefined)
  (define-predicate true? True)

  (define-type Literal (U Boolean Real Char String Bytes))
  (define-predicate literal? Literal)


  (define-type S-Exp  (U Literal Keyword Symbol S-List))
  (define-predicate s-exp?  S-Exp)
  (define-type S-List (Listof S-Exp))
  (define-predicate s-list? S-List)

  (define-type Ann-S-Exp (U Literal (List 'ann S-Exp Type)))
  (define-predicate ann-s-exp? Ann-S-Exp)

  (define-type Type (U Keyword Symbol Literal Types))
  (define-predicate type? Type)

  (define-type Types (Listof Type))
  (define-predicate types? Types)

  (define-type Prop (U 'Top 'Bot
                       Type (List '! Type)
                       (List    Type '@ (Rec Pos (U (List (Option Index)) (Pair (U 'car 'cdr) Pos))))
                       (List '! Type '@ (Rec Pos (U (List (Option Index)) (Pair (U 'car 'cdr) Pos))))
                       (Pair 'and     (Listof Prop))
                       (Pair 'or      (Listof Prop))
                       (Pair 'implies (Listof Prop))))
  (define-predicate prop? Prop)


  (define-type Lambda (U 'lambda 'λ))
  (define-predicate λ? Lambda)
  (define-predicate lambda? Lambda)

  (define-type Trace-Lambda (U 'trace-lambda 'trace-λ))
  (define-predicate trace-λ? Trace-Lambda)
  (define-predicate trace-lambda? Trace-Lambda)


  (: listof? (All (A) (case-> [-> (pred A) (pred (Listof A))]
                              [-> [-> Any Boolean] [-> Any Boolean]])))
  (define listof?
    (λ (pred)
      (λ (arg)
        (and (list? arg)
             (andmap pred arg)))))

  (: empty-list [-> Null])
  (define empty-list (λ () '()))

  (define-type (Queueof A) (List (Listof A) (Listof A)))
  (define-predicate queue? (Queueof Any))
  (define-predicate empty-queue? (Queueof Nothing))

  (: queueof? (All (A) (case-> [-> (pred A) (pred (Queueof A))]
                               [-> [-> Any Boolean] [-> Any Boolean]])))
  (define queueof?
    (λ (pred)
      (λ (arg)
        (and (queue? arg)
             ((listof? pred) (car  arg))
             ((listof? pred) (cadr arg))))))

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
                (match* (t1 t2)
                  [('Any _) t1]
                  [(_ 'Any) t2]
                  [('Nothing _) t2]
                  [(_ 'Nothing) t1]
                  [('True 'False) 'Boolean]
                  [('False 'True) 'Boolean]
                  [('Natural 'Real) 'Real]
                  [('Real 'Natural) 'Real]
                  [(_ _) 'Any])))]
      [(t1 t2 . ts)
       (and t1 t2 (types? ts)
            (for/fold ([res : Type (type-union t1 t2)])
                      ([t (in-list ts)])
              (type-union res t)))]))


  (: =: [-> Type Type Boolean])
  (define =:
    (λ (t1 t2)
      (let ([t1 (desugar-type t1)]
            [t2 (desugar-type t2)])
        (or (equal? t1 t2)
            (match* (t1 t2)
              [(`(Values ,As ...)
                `(Values ,Bs ...))
               #:when (and ((listof? type?) As)
                           ((listof? type?) Bs)
                           (= (length As) (length Bs)))
               (andmap =: As Bs)]
              [(`(Pair ,A1 ,B1) `(Pair ,A2 ,B2))
               (and (=: A1 A2) (=: B1 B2))]
              [(`(Listof ,A1) `(Listof ,A2))
               (=: A1 A2)]
              [(_ _) #f])))))

  (: >=: [-> Type Type Boolean])
  (define >=: (λ (t1 t2) (<=: t2 t1)))

  (: <=: [-> Type Type Boolean])
  (define <=: (λ (t1 t2) (or (=: t1 t2) (<: t1 t2))))

  (: >: [-> Type Type Boolean])
  (define >: (λ (t1 t2) (<: t2 t1)))

  (: <: [-> Type Type Boolean])         ; TODO
  (define <:
    (λ (t1 t2)
      (match* (t1 t2)
        [(_ 'Any) #t]
        [('Nothing _) #t]
        [('True  'Boolean) #t]
        [('False 'Boolean) #t]
        [('Natural 'Real) #t]
        [(`(Values ,As ... ,A* *)
          `(Values ,Bs ... ,B* *))
         #:when (and ((listof? type?) As)
                     (type? A*)
                     ((listof? type?) Bs)
                     (type? B*)
                     (<= (length As) (length Bs)))
         (and (andmap <=: As Bs)
              (<=: A* B*)
              (andmap (curry <=: A*) (list-tail Bs (length As))))]
        [(`(Values ,As ...)
          `(Values ,Bs ... ,B* *))
         #:when (and ((listof? type?) As)
                     (not (eq? '* (car (last-pair As))))
                     ((listof? type?) Bs)
                     (type? B*)
                     (>= (length As) (length Bs)))
         (and (andmap <=: As Bs)
              (andmap (curry >=: B*) (list-tail As (length Bs))))]
        [(`(Values ,As ...)
          `(Values ,Bs ...))
         #:when (and ((listof? type?) As)
                     ((listof? type?) Bs)
                     (= (length As) (length Bs)))
         (andmap <=: As Bs)]
        [(`(Pair ,A1 ,B1) `(Pair ,A2 ,B2))
         (and (not (=: t1 t2))
              (<=: A1 A2)
              (<=: B1 B2))]
        [(`(Listof ,A1) `(Listof ,A2))
         (<: A1 A2)]
        [(_ _) #f])))


  (: guess-type [-> S-Exp (Option Type)])
  (define guess-type
    (λ (exp)
      (match exp
        [`(,(or 'quote 'quasiquote)
           ,(? literal? atom))
         (guess-type atom)]
        [`(,(or 'quote 'quasiquote)
           ,(? symbol?))
         'Symbol]
        [(? true?)      'True]
        [(? false?)     'False]
        [(? natural?)   'Natural]
        [(? real?)      'Real]
        [(? string?)    'String]
        [(? bytes?)     'Bytes]
        [(? char?)      'Char]
        [(? null?)      'Null]
        [(? undefined?) 'Undefined]
        [`(ann  ,exp ,type) #:when (and (s-exp? exp) (type? type)) type]
        [`(cast ,exp ,type) #:when (and (s-exp? exp) (type? type)) type]
        [_ #f])))


  (: desugar-prop [-> Prop Prop])
  (define desugar-prop
    (let ()
      (define-predicate path-elem? (U 'car 'cdr))
      (λ (prop)
        (match prop
          [`(  ,type @ ,path-elems ... ,index)
           #:when (and (type? type)
                       ((listof? path-elem?) path-elems)
                       ((or/c index? false?) index))
           `(  ,(desugar-type type) @ ,@path-elems ,index)]
          [`(! ,type @ ,path-elems ... ,index)
           #:when (and (type? type)
                       ((listof? path-elem?) path-elems)
                       ((or/c index? false?) index))
           `(! ,(desugar-type type) @ ,@path-elems ,index)]
          [type       #:when (type? type) `(  ,type @ 0)]
          [`(! ,type) #:when (type? type) `(! ,type @ 0)]
          [_ prop]))))

  (: desugar-type [-> Type Type])
  (define desugar-type
    (λ (type)
      (match type
        [`(Values ,T) #:when (type? T) (desugar-type T)]


        [`(All () ,T) #:when (type? T) (desugar-type T)]
        [`(All (,A ,.. ,B ...) ,T)
         #:when (and (type? A)
                     (eq? .. '...)
                     ((listof? type?) B)
                     (type? T))
         `(All (A ...) ,(desugar-type `(All (,@B) ,T)))]
        [`(All (,A ,B ..1) ,T)
         #:when (and (type? A)
                     ((listof? type?) B)
                     (type? T))
         `(All (A) ,(desugar-type `(All (,@B) ,T)))]


        ['(List) 'Null]
        [`(List ,A0 ,A* ...)
         #:when (and (type? A0)
                     ((listof? type?) A*))
         `(Pair ,(desugar-type A0)
                ,(desugar-type `(List ,@A*)))]
        [`(List* ,A0 ,A1)
         #:when (and (type? A0)
                     (type? A1))
         `(Pair ,(desugar-type A0)
                ,(desugar-type A0))]
        [`(List* ,A0 ,A* ..1)
         #:when (and (type? A0)
                     ((listof? type?) A*))
         `(Pair ,(desugar-type A0)
                ,(desugar-type `(List* ,@A*)))]


        [`[-> (Values ,I ...) (Values ,O ...) : #:+ ,T #:- ,F]
         #:when (and ((listof? type?) I)
                     ((listof? type?) O)
                     (prop? T)
                     (prop? F))
         `[-> (Values ,@(map desugar-type I))
              (Values ,@(map desugar-type O))
              :
              #:+ ,(desugar-prop T)
              #:- ,(desugar-prop F)]]
        [`[-> ,I ... (Values ,O ...) : #:+ ,T #:- ,F]
         #:when (and ((listof? type?) I)
                     ((listof? type?) O)
                     (prop? T)
                     (prop? F))
         (desugar-type `[-> (Values ,@I) (Values ,@O) : #:+ ,T #:- ,F])]
        [`[-> ,I ... ,O : #:+ ,T #:- ,F]
         #:when (and ((listof? type?) I)
                     (type? O)
                     (prop? T)
                     (prop? F))
         (desugar-type `[-> (Values ,@I) (Values  ,O) : #:+ ,T #:- ,F])]
        [`[-> ,I ... ,O : #:- ,F #:+ ,T]
         #:when (and ((listof? type?) I)
                     (type? O)
                     (prop? T)
                     (prop? F))
         (desugar-type `[-> ,I ... ,O : #:+ ,T #:- ,F])]
        [`[-> ,I ... ,O : #:+ ,T]
         #:when (and ((listof? type?) I)
                     (type? O)
                     (prop? T))
         (desugar-type `[-> ,@I ,O : #:+ ,T #:- Top])]
        [`[-> ,I ... ,O : #:- ,F]
         #:when (and ((listof? type?) I)
                     (type? O)
                     (prop? F))
         (desugar-type `[-> ,@I ,O : #:+ Top #:- ,F])]
        [`[-> ,I ... ,O : ,P]
         #:when (and ((listof? type?) I)
                     (type? O)
                     (type? P))
         (desugar-type `[-> ,@I ,O : #:+ ,P #:- (! ,P)])]
        ['[->] (desugar-type '[-> (Values) (Values)])]
        [`[-> ,O]
         #:when (type? O)
         (desugar-type `[-> ,O : #:+ (Top @ #f) #:- (Top @ #f)])]
        [`[-> ,I ..1 ,O]
         #:when (and ((listof? type?) I)
                     (type? O))
         (desugar-type `[-> ,@I ,O : #:+ Top #:- Top])]


        ;; reduce
        [(? list?) (map desugar-type type)]
        [_ type])))


  (define-type Cont (Listof Frame))
  (define-predicate cont? Cont)
  (struct frame
    ([type : Symbol]
     [func : [-> Cont [-> ExpVal FinalAnswer]]])
    #:type-name Frame)


  (define-type DenVal (U Literal Symbol Keyword Undefined Void Null
                         Cont Primitive-Proc Proc Trace-Proc
                         (Queueof DenVal)

                         (Boxof     DenVal)
                         (Pairof    DenVal DenVal)
                         #;(MPairof   DenVal DenVal)
                         (Vectorof  DenVal)
                         (HashTable DenVal DenVal)))
  (define-type ExpVal DenVal)
  (define-new-subtype FinalAnswer (final-answer ExpVal))


  (define-struct env
    ([type  : (U 'empty-env 'extend-env)]
     [binds : (Immutable-HashTable Symbol (Boxof DenVal))])
    #:type-name Env)

  (define-struct tenv
    ([type  : (U 'empty-tenv 'extend-tenv)]
     [binds : (Immutable-HashTable Symbol Type)])
    #:type-name TEnv)

  (define-struct renv
    ([type  : (U 'empty-renv 'extend-renv)]
     [binds : (Immutable-HashTable Type Type)])
    #:type-name REnv)


  (define-struct primitive-proc
    ([λ : [-> DenVal * ExpVal]])
    #:type-name Primitive-Proc)

  (define-struct proc
    ([vars : (U Symbol (Listof Symbol))]  ; Symbol is used for `apply-primitive'.
     [body : Exp]
     [saved-env : Env])
    #:type-name Proc)

  (define-struct (trace-proc proc) () #:type-name Trace-Proc)

  (: thread-share-memory? (Parameter Boolean))
  (define thread-share-memory? (make-parameter #f))


  (: denval? [-> Any Boolean])
  (define denval?
    (λ (arg)
      (or (literal? arg)
          (symbol? arg)
          (keyword? arg)
          (undefined? arg)
          (void? arg)
          (null? arg)
          (primitive-proc? arg)
          (proc? arg)
          (trace-proc? arg)
          ((queueof? denval?) arg)

          (denbox? arg)
          (denpair? arg)
          #;(denmpair? arg)
          (denvector? arg)
          (denhash? arg))))

  (: denbox? [-> Any Boolean])
  (define denbox? (λ (arg) (and (box? arg) (denval? (unbox arg)))))

  (: denpair? [-> Any Boolean])
  (define denpair? (λ (arg) (and (pair? arg) (denval? (car arg)) (denval? (cdr arg)))))

  #;(: denmpair? [-> Any Boolean])
  #;(define denmpair? (λ (arg) (and (mpair? arg) (denval? (mcar arg)) (denval? (mcdr arg)))))

  (: denvector? [-> Any Boolean])
  (define denvector? (λ (arg) (and (vector? arg) (for/and ([i (in-vector arg)]) (denval? i)))))

  (: denmvector? [-> Any Boolean])
  (define denmvector? (λ (arg) (and (denvector? arg) (not (immutable? arg)))))

  (: denimvector? [-> Any Boolean])
  (define denimvector? (λ (arg) (and (denvector? arg) (immutable? arg))))

  (: denhash? [-> Any Boolean])
  (define denhash? (λ (arg) (and (hash? arg) (for/and ([(k v) (in-hash arg)]) (and (denval? k) (denval? v))))))

  (: denmhash? [-> Any Boolean])
  (define denmhash? (λ (arg) (and (denhash? arg) (not (immutable? arg)))))

  (: denimhash? [-> Any Boolean])
  (define denimhash? (λ (arg) (and (denhash? arg) (not (immutable? arg)))))


  (: expval? [-> Any Boolean])
  (define expval? (λ (arg) (denval? arg)))

  (: final-answer? [-> Any Boolean])
  (define final-answer? (λ (arg) (expval? arg)))


  (define-struct exp () #:transparent #:type-name Exp)


  (define-struct (ann-exp exp)
    ([exp  : Exp]
     [type : Type])
    #:transparent
    #:type-name Ann-Exp)

  (define-struct (cast-exp exp)
    ([exp  : Exp]
     [type : Type])
    #:transparent
    #:type-name Cast-Exp)

  (define-struct (inst-exp exp)
    ([exp   : Exp]
     [types : (Pair Type (Listof Type))])
    #:transparent
    #:type-name Inst-Exp)


  (define-struct (assign-exp exp)
    ([var : Symbol]
     [exp : Exp])
    #:transparent
    #:type-name Assign-Exp)


  (define-struct (quote-exp exp)
    ([datum : S-Exp])
    #:transparent
    #:type-name Quote-Exp)

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

  (define-struct (bytes-exp exp)
    ([bs : Bytes])
    #:transparent
    #:type-name Bytes-Exp)


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


  (define-struct (begin-exp exp)
    ([exps : (Pair Exp (Listof Exp))])
    #:transparent
    #:type-name Begin-Exp)


  (define-struct (new-closure-exp exp)
    ([exp : Exp])
    #:transparent
    #:type-name New-Closure-Exp)

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

  )

(require (except-in 'types
                     denval?
                     denbox?
                     denpair?
                     #;denmpair?
                     denvector?
                     denmvector?
                     denimvector?
                     denhash?
                     denmhash?
                     denimhash?

                     expval?
                     final-answer?))
(provide (all-from-out 'types))

(require typed/racket/unsafe)
(unsafe-require/typed/provide
 'types
 [denval?       (pred DenVal)]
 [denbox?       (pred (Boxof DenVal))]
 [denpair?      (pred (Pairof DenVal DenVal))]
 #;[denmpair?     (pred (MPairof DenVal DenVal))]
 [denvector?    (pred (Vectorof DenVal))]
 [denmvector?   (pred (Mutable-Vectorof DenVal))]
 [denimvector?  (pred (Immutable-Vectorof DenVal))]
 [denhash?      (pred (HashTable DenVal DenVal))]
 [denmhash?     (pred (Mutable-HashTable DenVal DenVal))]
 [denimhash?    (pred (Immutable-HashTable DenVal DenVal))]

 [expval?       (pred ExpVal)]
 [final-answer? (pred FinalAnswer)])
