#lang typed/racket

(require "../base/base.rkt")


(: *check-code* [-> S-Exp Env Namespace Boolean])
(define *check-code*
  (λ (code env eval-ns)
    (: eval-type (U Undefined Symbol))
    (define eval-type undefined)

    (with-handlers ([list? (ann (λ (msg)
                                  (displayln "Not Equal:")
                                  (pretty-print code)
                                  (pretty-print msg)
                                  (raise #f))
                                [-> (Listof Any) Nothing])]
                    [exn:fail? (ann (λ (ex)
                                      (displayln (format "Raise Error in ~a:" eval-type))
                                      (pretty-print code)
                                      (raise ex))
                                    [-> exn:fail Nothing])])
      (define-values (res output)
        (let ()
          (define o (open-output-string))
          (set! eval-type 'eval)
          (values (parameterize ([current-output-port o])
                    (call-with-values
                     (λ () (eval `(begin (require typed/racket/base) ,code) eval-ns))
                     (λ args (car args))))
                  (get-output-string o))))

      (define-values (*res* *output*)
        (let ()
          (define *o* (open-output-string))
          (set! eval-type '*eval*)
          (values (parameterize ([current-output-port *o*])
                    (*eval* code env (id-cont)))
                  (get-output-string *o*))))

      (if (and (equal?  res    *res*)
               (string=? output *output*))
          (cond
            #;[(eq? env (base-env))
               (define-values (+res+ +output+)
                 (let ()
                   (define +o+ (open-output-string))
                   (set! eval-type '+eval+)
                   (values (parameterize ([current-output-port +o+])
                             (*eval* `(eval ',code) env (id-cont)))
                           (get-output-string +o+))))

               (if (and (equal?  +res+    *res*)
                        (string=? +output+ *output*))
                   #t
                   (raise (list '(+eval+ *eval*)
                                (list (format "~a" +res+) +output+)
                                (list (format "~a" *res*) *output*))))]
            [else #t])
          (raise (list '(eval *eval*)
                       (list (format "~a" res)   output)
                       (list (format "~a" *res*) *output*)))))))


(define-namespace-anchor ns-anchor)
(define eval-ns (namespace-anchor->namespace ns-anchor))

(for ([eval-ns (in-list (list eval-ns))])
  (for ([op (in-list (list true?))])
    (namespace-set-variable-value! (assert (object-name op) symbol?) op #t eval-ns)))


(displayln "Start base test.\n")


(for ([i (in-naturals)]
      [code
       (in-list
        '(2
          -9
          (not #t)
          (not #f)
          (void)

          (+ 1 2 (- 3 4))
          (zero? (+ 1 2 (- 3 4)))
          (real? (void))

          (begin
            (define dio #f)
            (define noah "")
            (displayln noah)
            (set! noah "Noah Ma")
            (displayln noah))

          (begin
            (: fact [-> Real Real])
            (define fact
              (λ (n)
                (if (zero? n)
                    1
                    (* n (fact (sub1 n))))))
            (displayln (fact 0))
            (displayln (fact 1))
            (displayln (fact 2))
            (displayln (fact 3)))

          (begin
            (: pdisplayln (All (A) [-> A Void]))
            (define pdisplayln
              (λ (arg)
                (displayln arg)))

            ((inst pdisplayln Real) 123)
            ((inst pdisplayln String) "123")
            ((inst pdisplayln Symbol) '|123|)
            ((inst pdisplayln Any) '123))
          ))])
  (displayln (format "test ~a: ~a" i (*check-code* code (base-env) eval-ns))))
