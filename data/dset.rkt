#lang racket/base

(require racket/set
         racket/generic
         (only-in racket/unsafe/ops
                  unsafe-struct*-ref
                  unsafe-struct*-set!
                  unsafe-unbox*
                  unsafe-box*-cas!
                  unsafe-set-box*!
                  unsafe-vector*-ref)
         (for-syntax racket/base))




(provide dset?
         immutable-dset?
         mutable-dset?
         dset-equal?
         dset-eqv?
         dset-eq?
         (rename-out [dset* dset]
                     [dseteqv* dseteqv]
                     [dseteq* dseteq]
                     [mutable-dset* mutable-dset]
                     [mutable-dseteqv* mutable-dseteqv]
                     [mutable-dseteq* mutable-dseteq])
         list->dset
         list->dseteqv
         list->dseteq
         list->mutable-dset
         list->mutable-dseteqv
         list->mutable-dseteq
         in-dset
         for/dset
         for/dseteqv
         for/dseteq
         for*/dset
         for*/dseteqv
         for*/dseteq
         for/mutable-dset
         for/mutable-dseteqv
         for/mutable-dseteq
         for*/mutable-dset
         for*/mutable-dseteqv
         for*/mutable-dseteq
         dset-compact?
         dset-compact
         dset-compact!)


(define-syntax-rule (no-key-err-thunk fun-name key)
  (λ () (raise (make-exn:fail:contract
                (format "~a: no value found for key\n key: ~a" (quote fun-name) key)
                (current-continuation-marks)))))

;; standard "default value call if val is thunk else return val" behavior
(define-syntax-rule (default val)
  (let ([d val]) (if (procedure? d) (d) d)))

(define-syntax-rule (unbox-elem keybox)
  (weak-box-value keybox #f))

;; updates an immutable hash table and, if it's a new element,
;; add it to seq
(define (update-elems+seq elems seq elem)
  (let ([prev-count (hash-count elems)]
        [elems (hash-set elems elem #t)])
    (values elems (if (eqv? prev-count (hash-count elems))
                      seq
                      (cons (make-weak-box elem) seq)))))



(define (dset? x) (or (immutable-dset? x) (mutable-dset? x)))

;; constructor template for immutable dsets
(define-syntax-rule (immutable-dset-constructor name empty init-hash)
  (case-lambda
    [() empty]
    [args (build-immutable-dset (quote name) init-hash 0 '() args)]))

(define (build-immutable-dset name init-hash del init-seq initial-args)
  (let loop ([args initial-args]
             [elems init-hash]
             [seq init-seq])
    (cond
      [(pair? args)
       (let-values ([(elems seq) (update-elems+seq elems seq (car args))])
         (loop (cdr args) elems seq))]
      [(null? args) (idset elems del seq)]
      [else (error name "impossible! you found a bug!")])))

(define dset*    (immutable-dset-constructor dset empty-dset #hash()))
(define dseteqv* (immutable-dset-constructor dseteqv empty-dseteqv #hasheqv()))
(define dseteq*   (immutable-dset-constructor dseteq empty-dseteq #hasheq()))

;; constructor template for mutable dsets
(define-syntax-rule (mutable-dset-constructor name init-hash)
  (case-lambda
    [() (mdset init-hash 0 '())]
    [initial-args
     (let loop ([args initial-args]
                [elems init-hash]
                [seq '()])
       (cond
         [(pair? args)
          (let-values ([(elems seq) (update-elems+seq elems seq (car args))])
            (loop (cdr args) elems seq))]
         [(null? args) (mdset elems 0 seq)]
         [else (error (quote name) "impossible! you found a bug!")]))]))

(define mutable-dset* (mutable-dset-constructor mutable-dset #hash()))
(define mutable-dseteqv* (mutable-dset-constructor mutable-dseteqv #hasheqv()))
(define mutable-dseteq* (mutable-dset-constructor mutable-dseteq #hasheq()))


(define-for-syntax (parse-i-bindings dd-id elems-id del-id seq-id)
  (with-syntax ([dd dd-id]
                [elems elems-id]
                [del del-id]
                [seq seq-id])
    (append
     (if (eq? '_ (syntax->datum #'elems))
         (list)
         (list #'[elems (unsafe-struct*-ref dd 0)]))
     (if (eq? '_ (syntax->datum #'del))
         (list)
         (list #'[del (unsafe-struct*-ref dd 1)]))
     (if (eq? '_ (syntax->datum #'seq))
         (list)
         (list #'[seq (unsafe-struct*-ref dd 2)])))))

(define-for-syntax (parse-m-bindings dd-id elems-id del-id seq-id)
  (with-syntax ([dd dd-id]
                [elems elems-id]
                [del del-id]
                [seq seq-id]
                [content #`#,(gensym 'content)])
    (let ([bindings (append
                     (if (eq? '_ (syntax->datum #'elems))
                         (list)
                         (list #'[elems (unsafe-vector*-ref content 0)]))
                     (if (eq? '_ (syntax->datum #'del))
                         (list)
                         (list #'[del (unsafe-vector*-ref content 1)]))
                     (if (eq? '_ (syntax->datum #'seq))
                         (list)
                         (list #'[seq (unsafe-vector*-ref content 2)])))])
      (if (null? bindings)
          '()
          (cons #'[content (unsafe-unbox* (unsafe-struct*-ref dd 0))]
                bindings)))))

;; macro for defining functions whose first argument is a dset
;;
;; This automatically inserts a dset? check (or immutable/mutable-dset?)
;; and raise-argument-error for failure, as well as pattern matching
;; out the dset's fields quickly after the check is complete
(define-syntax (define/ds-match stx)
  (syntax-case stx (idset mdset)
    ;; immutable & mutable dset function
    [(_ (name ds . other-args)
        [(idset i-elems i-n i-seq) . i-body]
        [(mdset m-elems m-n m-seq) . m-body])
     (and (identifier? #'i-elems) (identifier? #'i-n) (identifier? #'i-seq)
          (identifier? #'m-elems) (identifier? #'m-n) (identifier? #'m-seq))
     (with-syntax ([i-bindings (parse-i-bindings #'ds #'i-elems #'i-n #'i-seq)]
                   [m-bindings (parse-m-bindings #'ds #'m-elems #'m-n #'m-seq)])
       (quasisyntax/loc stx
         (define (name ds . other-args)
           (cond
             [(immutable-dset? ds) #,(syntax/loc #'i-body (let i-bindings . i-body))]
             [(mutable-dset? ds) #,(syntax/loc #'m-body (let* m-bindings . m-body))]
             [else (raise-argument-error (quote name) "dset?" ds)]))))]
    ;; immutable dset function
    [(_ (name ds . other-args)
        [(idset elems n seq) . body])
     (and (identifier? #'elems) (identifier? #'n) (identifier? #'seq))
     (with-syntax ([bindings (parse-i-bindings #'ds #'elems #'n #'seq)])
       (quasisyntax/loc stx
         (define (name ds . other-args)
           (cond
             [(immutable-dset? ds) #,(syntax/loc #'body (let bindings . body))]
             [else (raise-argument-error (quote name) "immutable-dset?" ds)]))))]
    ;; only mutable dset function
    [(_ (name ds . other-args)
        [(mdset elems n seq) . body])
     (and (identifier? #'elems) (identifier? #'n) (identifier? #'seq))
     (with-syntax ([bindings (parse-m-bindings #'ds #'elems #'n #'seq)])
       (quasisyntax/loc stx
         (define (name ds . other-args)
           (cond
             [(mutable-dset? ds) #,(syntax/loc #'body (let* bindings . body))]
             [else (raise-argument-error (quote name) "mutable-dset?" ds)]))))]))

(define/ds-match (dset-equal? ds)
  [(idset elems _ _) (hash-equal? elems)]
  [(mdset elems _ _) (hash-equal? elems)])
(define/ds-match (dset-eqv? ds)
  [(idset elems _ _) (hash-eqv? elems)]
  [(mdset elems _ _) (hash-eqv? elems)])
(define/ds-match (dset-eq? ds)
  [(idset elems _ _) (hash-eq? elems)]
  [(mdset elems _ _) (hash-eq? elems)])


(define-syntax-rule (list->some-idset mk init-hash)
  (λ (l)
    (define-values (elems seq)
           (for/fold ([elems init-hash]
                      [seq '()])
                     ([elem (in-list l)])
             (update-elems+seq elems seq elem)))
    (mk elems 0 seq)))

(define list->dset (list->some-idset idset #hash()))
(define list->dseteqv (list->some-idset idset #hasheqv()))
(define list->dseteq (list->some-idset idset #hasheq()))
(define list->mutable-dset (list->some-idset mdset #hash()))
(define list->mutable-dseteqv (list->some-idset mdset #hasheqv()))
(define list->mutable-dseteq (list->some-idset mdset #hasheq()))



;;
;; dset-compact?
;;
(define/ds-match (dset-compact? dd)
  [(idset _ del _) (eqv? 0 del)]
  [(mdset _ del _) (eqv? 0 del)])

;;
;; dset-compact
;;
(define/ds-match (dset-compact ds)
  [(idset elems del seq)
   (cond
     [(eqv? 0 del) ds]
     [else (idset elems 0 (for*/list ([elemb (in-list seq)]
                                      [key (in-value (unbox-elem elemb))]
                                      #:when (hash-has-key? elems key))
                            key))])])

;;
;; dset-compact!
;;
(define/ds-match (dset-compact! mds)
  [(mdset elems del seq)
   (unless (eqv? 0 del)
     (let ([seq (for*/list ([elemb (in-list seq)]
                            [elem (in-value (unbox-elem elemb))]
                            #:when (hash-has-key? elems elem))
                  elem)])
       (unless (try-update-mdset-content! mds elems 0 seq)
         (dset-compact! mds))))])

(define-syntax-rule (assert-compatible-hashes-err name ds1 ds2 elems1 elems2)
  (cond [(hash-equal? elems1)
         (unless (hash-equal? elems2) (raise-incompatible-hash-error name ds1 ds2))]
        [(hash-eqv? elems1)
         (unless (hash-eqv? elems2) (raise-incompatible-hash-error name ds1 ds2))]
        [else ;; hasheq
         (unless (hash-eq? elems2) (raise-incompatible-hash-error name ds1 ds2))]))

(define-syntax-rule (raise-incompatible-hash-error name ds1 ds2)
  (raise (make-exn:fail:contract
          (format "name: ~a\n dset 1: ~a\n dset 2: ~a"
                  "given dsets do not use the same key comparison:"
                  name
                  ds1
                  ds2)
          (current-continuation-marks))))


;;
;; next-element
;;
(define-syntax-rule (next-elem ds seq)
  (let ([elems (dset-elems ds)])
    (cond
      [(pair? seq)
       (define elem (unbox-elem (car seq)))
       (if (hash-ref elems elem #f)
           (values elem (cdr seq))
           (next-elem-proc elems (cdr seq)))]
      [else (values #f #f)])))

(define (next-elem-proc elems seq)
  (cond
    [(pair? seq)
     (define elem (unbox-elem (car seq)))
     (if (hash-ref elems elem #f)
         (values elem (cdr seq))
         (next-elem-proc elems (cdr seq)))]
    [else (values #f #f)]))

;;
;; in-dset-proc
;;
(define ((in-dset-proc name pred? pred-str) ds)
  (cond
    [(pred? ds) (set->list ds)]
    [else (raise-argument-error name pred-str ds)]))

;;
;; in-dset
;;
(define-sequence-syntax in-dset
  (λ () #'(in-dset-proc 'in-dset dset? "dset?"))
  (λ (stx)
    (syntax-case stx ()
      [[(elem) (_ ds-exp)]
       #'[(elem)
          (:do-in
           ;; ([(outer-id ...) outer-expr] ...)
           ([(ds seq) (let ([ds ds-exp])
                        (unless (dset? ds)
                          (raise-argument-error 'in-dset "dset?" ds))
                        (values ds (dset-elems ds)))])
           ;; outer-check
           #t
           ;; ([loop-id loop-expr] ...)
           ([pos seq])
           ;; pos-guard
           #t
           ;; ([(inner-id ...) inner-expr] ...)
           ([(elem rst) (next-elem ds pos)])
           ;; pre-guard
           rst
           ;; post-guard
           #t
           ;; (loop-arg ...)
           (rst))]]
      [[xs (_ dd-exp)]
       (list? (syntax->datum #'xs))
       (raise-syntax-error 'in-dset
                           (format "expected an identifier, given ~a"
                                   (length (syntax->list #'xs)))
                           #'xs)]
      [blah (raise-syntax-error 'in-dset "invalid usage" #'blah)])))


(define-syntax-rule (define-for-dset for-name for/derived mk empty-hash)
  (define-syntax (for-name stx)
    (syntax-case stx ()
      [(_ clauses . defs+exprs)
       (with-syntax ([original stx])
         (syntax/loc stx
           (let-values
               ([(elems seq)
                 (for/derived original
                   ([elems empty-hash]
                    [seq '()])
                   clauses
                   (let ([elem (let () . defs+exprs)])
                     (update-elems+seq elems seq elem)))])
             (mk elems 0 seq))))])))



(define-for-dset for/dset             for/fold/derived  idset #hash())
(define-for-dset for/dseteqv          for/fold/derived  idset #hasheqv())
(define-for-dset for/dseteq           for/fold/derived  idset #hasheq())
(define-for-dset for*/dset            for*/fold/derived idset #hash())
(define-for-dset for*/dseteqv         for*/fold/derived idset #hasheqv())
(define-for-dset for*/dseteq          for*/fold/derived idset #hasheq())
(define-for-dset for/mutable-dset     for/fold/derived  mdset #hash())
(define-for-dset for/mutable-dseteqv  for/fold/derived  mdset #hasheqv())
(define-for-dset for/mutable-dseteq   for/fold/derived  mdset #hasheq())
(define-for-dset for*/mutable-dset    for*/fold/derived mdset #hash())
(define-for-dset for*/mutable-dseteqv for*/fold/derived mdset #hasheqv())
(define-for-dset for*/mutable-dseteq  for*/fold/derived mdset #hasheq())



;; union one immutable dset with another -- if on is larger,
;; favor that one as the starting point to reduce costs
(define/ds-match (union-elems+seq-with-immutable-dset ds elems del seq)
  [(idset elems* del* seq*)
   (cond
     [(> (hash-count elems*) (hash-count elems))
      (union-helper elems* del* seq* elems seq)]
     [else (union-helper elems del seq elems* seq*)])])

(define (union-helper elems del seq elems* seq*)
  (let-values ([(elems seq) (for*/fold ([elems elems]
                                        [seq seq])
                                       ([elemb (in-list seq*)]
                                        [elem (in-value (unbox-elem elemb))]
                                        #:when (hash-ref elems* elem #f))
                              (define pre-count (hash-count elems))
                              (define new-elems (hash-set elems elem #t))
                              (if (eqv? pre-count (hash-count new-elems))
                                  (values elems seq)
                                  (values new-elems (cons elemb seq))))])
    (values elems del seq)))

(define-syntax-rule (too-fragmented? elems del)
  (>= del (hash-count elems)))

(define-syntax-rule (filter-seq elems seq)
  (for/list ([keyb (in-list seq)]
             #:when (hash-ref elems (unbox-elem keyb) #f))
    keyb))

(define-syntax-rule (raise-incompatible-sets-error name set1 set2)
  (raise (make-exn:fail:contract
          (format "~a: set arguments have incompatible equivalence predicate\n~a~a"
                  'set-intersect
                  (format "  first set: ~a\n" set1)
                  (format "  incompatible set: ~a\n" set2))
          (current-continuation-marks))))

(define (dset-elems ds)
  (cond
    [(immutable-dset? ds) (unsafe-immutable-dset-elems ds)]
    [(mutable-dset? ds) (unsafe-mutable-dset-elems ds)]
    [else (error 'dset-elems "impossible! you found a bug!")]))

;; 
;; idset=?
;; 
(define (idset=? ds1 ds2 rec-equal?)
  (rec-equal? (immutable-dset-elems ds1)
              (immutable-dset-elems ds2)))

;; 
;; dset-hash-code
;; 
(define (idset-hash-code dd rec-hc)
  (rec-hc (immutable-dset-elems dd)))


;; 
;; idset-print
;; 
(define (idset-print ds port mode)
  (cond
    [(dset-equal? ds)
     (if mode
         (write-string "#<dset: " port)
         (write-string "(dset " port))]
    [(dset-eqv? ds)
     (if mode
         (write-string "#<dseteqv: " port)
         (write-string "(dseteqv " port))]
    [else (if mode
              (write-string "#<dseteq: " port)
              (write-string "(dseteq " port))])
  (let ([recur (case mode
                 [(#t) write]
                 [(#f) display]
                 [else (λ (p port) (print p port mode))])])
    (recur (set->list ds) port))
  (if mode
      (write-string ">" port)
      (write-string ")" port)))

(struct immutable-dset (elems del seq)
  #:constructor-name idset
  #:methods gen:equal+hash
  [(define equal-proc idset=?)
   (define hash-proc idset-hash-code)
   (define hash2-proc idset-hash-code)]
  #:methods gen:custom-write
  [(define write-proc idset-print)]
  #:methods gen:set
  [;; set-member?
   (define/ds-match (set-member? ds elem)
     [(idset elems _ _) (hash-ref elems elem #f)])
   ;; set-add
   (define/ds-match (set-add ds elem)
     [(idset elems del seq)
      (let-values ([(elems seq) (update-elems+seq elems seq elem)])
        (idset elems del seq))])
   ;; set-remove
   (define/ds-match (set-remove ds elem)
     [(idset elems del seq)
      (let ([elems (hash-remove elems elem)])
        (if (too-fragmented? elems del)
            (idset elems 0 (filter-seq elems seq))
            (idset elems (add1 del) seq)))])
   ;; set-empty?
   (define/ds-match (set-empty? ds)
     [(idset elems _ _) (hash-empty? elems)])
   ;; set-count
   (define/ds-match (set-count ds)
     [(idset elems _ _) (hash-empty? elems)])
   ;; set-first
   (define/ds-match (set-first ds)
     [(idset elems _ seq)
      (define-values (elem rst) (next-elem ds seq))
      (unless rst
        (raise-argument-error 'set-first "(and/c set? (not/c set-empty?))" ds))
      elem])
   ;; set-rest
   (define/ds-match (set-rest ds)
     [(idset elems del seq)
      (define-values (elem rst) (next-elem ds seq))
      (unless rst
        (raise-argument-error 'dset-rest "(and/c set? (not/c set-empty?))" ds))
      (idset (hash-remove elems elem) del rst)])
   ;; set-copy-clear
   (define/ds-match (set-copy-clear ds)
     [(idset elems _ _) (idset (hash-clear elems) 0 '())])
   ;; subset?
   (define/ds-match (subset? ds1 ds2)
     [(idset elems1 _ _)
      (unless (immutable-dset? ds2)
        (raise-argument-error 'subset?
                              "immutable-dset?"
                              ds2))
      (define elems2 (immutable-dset-elems ds2))
      (assert-compatible-hashes-err 'subset? ds1 ds2 elems1 elems2)
      (hash-keys-subset? elems1 elems2)])
   (define/ds-match (set-clear ds)
     [(idset elems _ _) (idset (hash-clear elems) 0 '())])
   ;; set-union
   (define/ds-match (set-union ds . sets)
     [(idset elems del seq)
      (for ([set (in-list sets)])
        (unless (dset? set) (raise-incompatible-sets-error 'set-union ds set)))
      (let-values ([(elems del seq)
                    (for*/fold ([elems elems]
                                [del del]
                                [seq seq])
                               ([ds (in-list sets)])
                      (if (immutable-dset? ds)
                          (union-elems+seq-with-immutable-dset ds elems del seq)
                          (let-values ([(elems seq)
                                        (for/fold ([elems elems]
                                                   [seq seq])
                                                  ([elem (in-dset ds)])
                                          (update-elems+seq elems seq elem))])
                            (values elems del seq))))])
        (idset elems del seq))])
   ;; set-intersect
   (define/ds-match (set-intersect ds . sets)
     [(idset elems del seq)
      (for ([set (in-list sets)])
        (unless (dset? set) (raise-incompatible-sets-error 'set-intersect ds set)))
      (let-values ([(elems del)
                    (for/fold ([elems elems]
                               [del del])
                              ([elem (in-dset ds)])
                      (if (for/and ([ds (in-list sets)])
                            (hash-ref (dset-elems ds) elem #f))
                          (values elems del)
                          (values (hash-remove elems elem) (add1 del))))])
        (if (too-fragmented? elems del)
            (idset elems 0 (filter-seq elems seq))
            (idset elems del seq)))])
   ;; set-subtract
   (define/ds-match (set-subtract ds . sets)
     [(idset elems del seq)
      (for ([set (in-list sets)])
        (unless (dset? set) (raise-incompatible-sets-error 'set-subtract ds set)))
      (let-values ([(elems del)
                    (for*/fold ([elems elems]
                                [del del])
                               ([set (in-list sets)]
                                [elem (in-dset set)])
                      (define elems* (hash-remove elems elem))
                      (if (eqv? (hash-count elems) (hash-count elems*))
                          (values elems del)
                          (values elems* (add1 del))))])
        (if (too-fragmented? elems del)
            (idset elems 0 (filter-seq elems seq))
            (idset elems del seq)))])
   ;; set=?
   (define/ds-match (set=? ds1 ds2)
     [(idset elems1 _ _)
      (unless (dset? ds2) (raise-incompatible-sets-error 'set=? ds1 ds2))
      (define elems2 (dset-elems ds2))
      (or (equal? elems1 elems2)
          (and (hash-keys-subset? ds1 ds2)
               (hash-keys-subset? ds2 ds1)))])
   ;; set->list
   (define/ds-match (set->list ds)
     [(idset elems _ seq)
      (for*/list ([elemb (in-list seq)]
                  [elem (in-value (unbox-elem elemb))]
                  #:when (hash-ref elems elem #f))
        elem)])
   ;; set-map
   (define/ds-match (set-map ds f)
     [(idset elems _ seq)
      (for*/list ([elemb (in-list seq)]
                  [elem (in-value (unbox-elem elemb))]
                  #:when (hash-ref elems elem #f))
        (f elem))])
   ;; set-for-each
   (define/ds-match (set-for-each ds f)
     [(idset elems _ seq)
      (for* ([elemb (in-list seq)]
             [elem (in-value (unbox-elem elemb))]
             #:when (hash-ref elems elem #f))
        (f elem))])
   ;; in-set
   (define in-set in-dset)])

(define (unsafe-immutable-dset-elems dd)
  (unsafe-struct*-ref dd 0))
(define (unsafe-immutable-dset-seq dd)
  (unsafe-struct*-ref dd 2))



;;
;; mdset=?
;;
(define (mdset=? ds1 ds2 rec-equal?)
  (rec-equal? (unsafe-vector*-ref (unsafe-unbox* (mutable-dset-content-box ds1)) 0)
              (unsafe-vector*-ref (unsafe-unbox* (mutable-dset-content-box ds2)) 0)))

;;
;; mdset-hash-code
;;
(define (mdset-hash-code dd rec-hc)
  (rec-hc (content-elems (unbox (mutable-dset-content-box dd)))))


;; 
;; mdset-print
;; 
(define (mdset-print ds port mode)
  (cond
    [(dset-equal? ds)
     (if mode
         (write-string "#<mutable-dset: " port)
         (write-string "(mutable-dset " port))]
    [(dset-eqv? ds)
     (if mode
         (write-string "#<mutable-dseteqv: " port)
         (write-string "(mutable-dseteqv " port))]
    [else (if mode
              (write-string "#<mutable-dseteq: " port)
              (write-string "(mutable-dseteq " port))])
  (let ([recur (case mode
                 [(#t) write]
                 [(#f) display]
                 [else (λ (p port) (print p port mode))])])
    (recur (set->list ds) port))
  (if mode
      (write-string ">" port)
      (write-string ")" port)))

(struct mutable-dset (content-box)
  #:constructor-name unsafe-mk-mdset
  #:methods gen:equal+hash
  [(define equal-proc mdset=?)
   (define hash-proc mdset-hash-code)
   (define hash2-proc mdset-hash-code)]
  #:methods gen:custom-write
  [(define write-proc mdset-print)]
  #:methods gen:set
  ;; set-member?
  [(define/ds-match (set-member? ds elem)
     [(mdset elems _ _) (hash-ref elems elem #f)])
   ;; set-add!
   (define/ds-match (set-add! mds elem)
     [(mdset elems del seq)
      (let-values ([(elems seq) (update-elems+seq elems seq elem)])
        (unless (try-update-mdset-content! mds elems del seq)
          (set-add! mds elem)))])
   ;; set-remove!
   (define/ds-match (set-remove! mds elem)
     [(mdset elems del seq)
      (let ([elems (hash-remove elems elem)])
        (cond
          [(too-fragmented? elems del)
           (unless (try-update-mdset-content! mds elems 0 (filter-seq elems seq))
             (set-remove! mds elem))]
          [else
           (unless (try-update-mdset-content! mds elems (add1 del) seq)
             (set-remove! mds elem))]))])
   ;; set-union!
   (define/ds-match (set-union! mds . sets)
     [(mdset _ _ _)
      (for ([set (in-list sets)])
        (unless (dset? set) (raise-incompatible-sets-error 'set-union! mds set))
        (for ([elem (in-dset set)])
          (set-add! mds elem)))])
   ;; set-intersect!
   (define/ds-match (set-intersect! mds . sets)
     [(mdset _ _ _)
      (for ([set (in-list sets)])
        (unless (dset? set) (raise-incompatible-sets-error 'set-intersect! mds set)))
      (for ([elem (in-dset mds)])
        (unless (for/and ([ds (in-list sets)])
                  (hash-ref (dset-elems ds) elem #f))
          (set-remove! mds elem)))])
   ;; set-subtract!
   (define/ds-match (set-subtract! mds . sets)
     [(mdset _ _ _)
      (for ([set (in-list sets)])
        (unless (dset? set) (raise-incompatible-sets-error 'set-subtract! mds set))
        (for ([elem (in-dset set)])
          (set-remove! mds elem)))])
   ;; subset?
   (define/ds-match (subset? ds1 ds2)
     [(mdset elems1 _ _)
      (unless (dset? ds2) (raise-incompatible-sets-error 'subset? ds1 ds2))
      (define elems2 (dset-elems ds2))
      (assert-compatible-hashes-err 'subset? ds1 ds2 elems1 elems2)
      (hash-keys-subset? elems1 elems2)])
   ;; set-clear!
   (define/ds-match (set-clear! mds)
     [(mdset elems _ _)
      (fore-update-mdset-content! mds (hash-clear elems) 0 '())])
   ;; set-copy-clear
   (define/ds-match (set-copy-clear mds)
     [(mdset elems _ _) (mdset (hash-clear elems) 0 '())])
   ;; set-count
   (define/ds-match (set-count ds)
     [(mdset elems _ _) (hash-count elems)])
   ;; set-first
   (define/ds-match (set-first ds)
     [(mdset elems del seq)
      (define-values (elem rst) (next-elem ds seq))
      (unless rst
        (raise-argument-error 'set-first "(and/c set? (not/c set-empty?))" ds))
      elem])
   ;; set->list
   (define/ds-match (set->list ds)
     [(mdset elems _ seq)
      (for*/list ([elemb (in-list seq)]
                  [elem (in-value (unbox-elem elemb))]
                  #:when (hash-ref elems elem #f))
        elem)])
   ;; in-set
   (define in-set in-dset)
   ;; set-map
   (define/ds-match (set-map ds f)
     [(mdset _ _ seq)
      (for*/list ([elemb (in-list seq)]
                  [elem (in-value (unbox-elem elemb))]
                  #:when (hash-ref (unsafe-mutable-dset-elems ds) elem #f))
        (f elem))])
   ;; for-each
   (define/ds-match (set-for-each ds f)
     [(mdset _ _ seq)
      (for* ([elemb (in-list seq)]
             [elem (in-value (unbox-elem elemb))]
             #:when (hash-ref (unsafe-mutable-dset-elems ds) elem #f))
        (f elem))])])

(define (unsafe-mutable-dset-elems mdd)
  (unsafe-vector*-ref (unsafe-unbox* (unsafe-struct*-ref mdd 0)) 0))
(define (unsafe-mutable-dset-seq mdd)
  (unsafe-vector*-ref (unsafe-unbox* (unsafe-struct*-ref mdd 0)) 2))

;; NOTE: we assume this vector is of length 3 w/ unsafe ops,
;; change any/all unsafe-vector... operations if this is modified
(define-syntax-rule (content a b c) (vector-immutable a b c))

;; NOTE: we assume this structure for mdsets (i.e. that it contains
;; a box which contains a 'content-vector' -- if any of this is changed,
;; all unsafe ops must also be changed)
(define-syntax-rule (mdset elems del seq)
  (unsafe-mk-mdset (box (content elems del seq))))

(define (content-elems c) (vector-ref c 0))
;(define (content-del c) (vector-ref c 1))
;(define (content-seq c) (vector-ref c 2))

(define-syntax-rule (try-update-mdset-content! mdd elems del seq)
  (let* ([content-box (mutable-dset-content-box mdd)]
         [orig-content (unsafe-unbox* content-box)]
         [new-content (content elems del seq)])
    (unsafe-box*-cas! content-box orig-content new-content)))

(define (fore-update-mdset-content! mdd elems del seq)
  (define content-box (mutable-dset-content-box mdd))
  (unsafe-set-box*! content-box (content elems del seq)))

;; NOTE: keep these in sync w/ above defs!!!!!!
(define empty-dset (idset #hash() 0 '()))
(define empty-dseteqv (idset #hasheqv() 0 '()))
(define empty-dseteq (idset #hasheq() 0 '()))

