#lang racket/base

(require racket/unsafe/ops
         (for-syntax racket/base))

(provide dset?
         dset-equal?
         dset-eqv?
         dset-eq?
         immutable-dset?
         mutable-dset?
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
         dset-add
         dset-add!
         dset-member?
         dset-remove
         dset-remove!
         dset-copy
         dset-compact?
         dset-compact!
         dset-empty?
         dset-count
         dset->list
         dset-union
         dset-union*
         dset-union!
         dset-union*!
         dset-intersect
         dset-intersect*
         dset-intersect!
         dset-intersect*!
         dset-map
         dset-for-each
         in-dset
         for/dset
         for/dseteqv
         for/dseteq
         for/mutable-dset
         for/mutable-dseteqv
         for/mutable-dseteq
         for*/dset
         for*/dseteqv
         for*/dseteq
         for*/mutable-dset
         for*/mutable-dseteqv
         for*/mutable-dseteq)

(define-syntax-rule (in? elems)
  (λ (x) (hash-ref elems x #f)))

(define-syntax-rule (too-fragmented? elems del)
  (> del (arithmetic-shift (hash-count elems) -1)))

(define-syntax-rule (zero? n) (eqv? 0 n))

;; elems - hash? - the actual dictionary data structure -- used for
;; storing the members of the set
;; del - exact-nonnegative-integer? - how many deleted items are stored in 'seq'
;; seq - list? - the sequence of elements in LIFO order
;; NOTE - fields are mutable, but only the mutable-dset
;; functions should mutate the fields, and only for
;; mutable-ddicts
(struct dset (elems [del #:mutable] [seq #:mutable])
  #:constructor-name do-not-use-me-ever)

;; NOTE: keep these in sync w/ above def!!!!!!
;(define-syntax-rule (unsafe-dset-elems ds) (unsafe-struct*-ref ds 0))
(define-syntax-rule (unsafe-dset-elems ds) (dset-elems ds))

;(define-syntax-rule (unsafe-dset-del ds)          (unsafe-struct*-ref  ds 1))
(define-syntax-rule (unsafe-dset-del ds)          (dset-del  ds))

;(define-syntax-rule (unsafe-set-dset-del! ds val) (unsafe-struct*-set! ds 1 val))
(define-syntax-rule (unsafe-set-dset-del! ds val) (set-dset-del! ds val))

;(define-syntax-rule (unsafe-dset-seq ds)          (unsafe-struct*-ref  ds 2))
(define-syntax-rule (unsafe-dset-seq ds)          (dset-seq  ds))

;(define-syntax-rule (unsafe-set-dset-seq! ds val) (unsafe-struct*-set! ds 2 val))
(define-syntax-rule (unsafe-set-dset-seq! ds val) (set-dset-seq! ds val))

;; 
;; dset-print
;; 
(define (dset-print ds port mode)
  (define elems (dset-elems ds))
  (if mode
      (if (immutable-dset? ds)
          (write-string "#<dset:" port)
          (write-string "#<mutable-dset:" port))
      (if (immutable-dset? ds)
          (write-string "(dset" port)
          (write-string "(mutable-dset" port)))
  (let ([l (filter (in? elems) (dset-seq ds))]
        [recur (case mode
                 [(#t) write]
                 [(#f) display]
                 [else (λ (p port) (print p port mode))])])
    (when (not (null? l))
      (for* ([key (in-list l)])
        (write-string " " port)
        (recur key port))))
  (if mode
      (write-string ">" port)
      (write-string ")" port)))

;; 
;; dset=?
;; 
(define (dset=? ds1 ds2 rec-equal?)
  (rec-equal? (unsafe-dset-elems ds1)
              (unsafe-dset-elems ds2)))

;; 
;; dset-hash-code
;; 
(define (dset-hash-code ds rec-hc)
  (rec-hc (unsafe-dset-elems ds)))

(struct immutable-dset dset ()
  #:methods gen:equal+hash
  [(define equal-proc dset=?)
   (define hash-proc dset-hash-code)
   (define hash2-proc dset-hash-code)]
  #:methods gen:custom-write
  [(define write-proc dset-print)])

(struct mutable-dset dset ()
  #:methods gen:equal+hash
  [(define equal-proc dset=?)
   (define hash-proc dset-hash-code)
   (define hash2-proc dset-hash-code)]
  #:methods gen:custom-write
  [(define write-proc dset-print)])

(define empty-dset (immutable-dset #hash() 0 '()))
(define empty-dseteqv (immutable-dset #hasheqv() 0 '()))
(define empty-dseteq (immutable-dset #hasheq() 0 '()))

;; constructor template for immutable dsets
(define-syntax-rule (immutable-dset-constructor name empty init-hash)
  (case-lambda
    [() empty]
    [args (build-immutable-dset (quote name) init-hash 0 '() args)]))

(define (build-immutable-dset name init-hash del init-seq initial-args)
  (let loop ([args initial-args]
             [elems init-hash]
             [count (hash-count init-hash)]
             [seq init-seq])
    (cond
      [(pair? args)
       (let* ([elem (car args)]
              [elems (hash-set elems elem #t)])
         (cond
           [(eqv? (hash-count elems) count)
            (loop (cdr args) elems count seq)]
           [else (loop (cdr args) elems (add1 count) (cons elem seq))]))]
      [(null? args) (immutable-dset elems del seq)]
      [else
       (raise-argument-error
        name
        "a list of elements"
        initial-args)])))

(define dset*     (immutable-dset-constructor dset empty-dset #hash()))
(define dseteqv*  (immutable-dset-constructor dseteqv empty-dseteqv #hasheqv()))
(define dseteq*   (immutable-dset-constructor dseteq empty-dseteq #hasheq()))

;; constructor template for mutable dsets
(define-syntax-rule (mutable-dset-constructor name make-init-hash)
  (case-lambda
    [() (mutable-dset (make-init-hash) 0 '())]
    [args (define elems (make-init-hash))
          (define ds (mutable-dset elems 0 '()))
          (add-to-mutable-dset! (quote name) ds elems 0 '() args)
          ds]))

(define (add-to-mutable-dset! name ds elems init-count init-seq initial-args)
  (let loop ([args initial-args]
             [count init-count]
             [seq init-seq])
    (cond
      [(pair? args)
       (define key (car args))
       (hash-set! elems key #t)
       (cond
         [(eqv? (hash-count elems) count)
          (loop (cdr args) count seq)]
         [else (loop (cdr args) (add1 count) (cons key seq))])]
      [(null? args) (unsafe-set-dset-seq! ds seq)]
      [else
       (raise-argument-error
        name
        "a list of elements"
        initial-args)])))

(define mutable-dset*    (mutable-dset-constructor mutable-dset make-hash))
(define mutable-dseteqv* (mutable-dset-constructor mutable-dseteqv make-hasheqv))
(define mutable-dseteq*  (mutable-dset-constructor mutable-dseteq make-hasheq))


;; "make-" constructor template for immutable dsets
(define-syntax-rule (list->dset/template name init-hash)
  (λ (initial-l)
    (let loop ([l initial-l]
               [elems init-hash]
               [count 0]
               [seq '()])
      (cond
        [(pair? l)
         (define elem (car l))
         (let ([elems (hash-set elem #t)])
           (if (eqv? (hash-count elems) count)
               (loop (cdr l) elems count seq)
               (loop (cdr l) elems (add1 count) (cons elem seq))))]
        [(null? l) (immutable-dset elems 0 seq)]
        [else (raise-argument-error (quote name)
                                    "a list of elements"
                                    initial-l)]))))

(define list->dset (list->dset/template list->dset #hash()))
(define list->dseteqv (list->dset/template list->dseteqv #hasheqv()))
(define list->dseteq (list->dset/template list->dseteq #hasheq()))

;; "make-" constructor template for mutable dsets
(define-syntax-rule (list->mutable-dset/template name make-init-hash)
  (λ (initial-l)
    (define elems (make-init-hash))
    (let loop ([l initial-l]
               [count 0]
               [seq '()])
      (cond
        [(pair? l)
         (define elem (car l))
         (let ([elems (hash-set! elem #t)])
           (if (eqv? (hash-count elems) count)
               (loop (cdr l) count seq)
               (loop (cdr l) (add1 count) (cons elem seq))))]
        [(null? l) (mutable-dset elems 0 seq)]
        [else (raise-argument-error (quote name)
                                    "a list of elements"
                                    initial-l)]))))

(define list->mutable-dset (list->mutable-dset/template list->mutable-dset make-hash))
(define list->mutable-dseteqv (list->mutable-dset/template list->mutable-dseteqv make-hasheqv))
(define list->mutable-dseteq (list->mutable-dset/template list->mutable-dseteq make-hasheq))

;; macro for defining functions whose first argument is a dset
;;
;; This automatically inserts a dset? check (or immutable/mutable-dset?)
;; and raise-argument-error for failure, as well as pattern matching
;; out the dset's fields if the check passes
(define-syntax (define/ds stx)
  (syntax-case stx ()
    [(_ (name [(dset-spec elems del seq) ds] . other-args) . body)
     (memq (syntax->datum #'dset-spec) '(dset idset mdset))
     (with-syntax
         ;; bind all non-wildcard fields
         ([dset-pred
           (case (syntax->datum #'dset-spec)
             [(dset) #'dset?]
             [(idset) #'immutable-dset?]
             [(mdset) #'mutable-dset?])]
          [dset-pred-str
           (case (syntax->datum #'dset-spec)
             [(dset) #'"dset?"]
             [(idset) #'"immutable-dset?"]
             [(mdset) #'"mutable-dset?"])])
       (with-syntax
           ([bindings (append
                       (if (eq? '_ (syntax->datum #'elems))
                           (list)
                           (list #'[elems (unsafe-dset-elems ds)]))
                       (if (eq? '_ (syntax->datum #'del))
                           (list)
                           (list #'[del (unsafe-dset-del ds)]))
                       (if (eq? '_ (syntax->datum #'seq))
                           (list)
                           (list #'[seq (unsafe-dset-seq ds)])))]
            ;; build a reasonable error message if not given a dset
            ;; as the 1st argument
            [error-expr
             (if (identifier? #'other-args)
                 ;; rest args
                 (syntax/loc stx
                   (raise-argument-error
                    (quote name) dset-pred-str 0 ds other-args))
                 ;; no rest args
                 (quasisyntax/loc stx
                   (raise-argument-error
                    (quote name) dset-pred-str 0 ds
                    ;; grab argument ids to report as other args
                    . #,(for/fold ([others #'()])
                                  ([arg (in-list (reverse (syntax->list #'other-args)))])
                          (syntax-case arg ()
                            [[id def-val] (identifier? #'id) #`(id . #,others)]
                            [id (identifier? #'id) #`(id . #,others)])))))])
         (syntax/loc stx
           (define (name ds . other-args)
             (cond
               [(dset-pred ds)
                (let bindings . body)]
               [else error-expr])))))]))


(define (dset-equal? x)
  (and (dset? x) (hash-equal? (unsafe-dset-elems x))))

(define (dset-eqv? x)
  (and (dset? x) (hash-eqv? (unsafe-dset-elems x))))

(define (dset-eq? x)
  (and (dset? x) (hash-eq? (unsafe-dset-elems x))))


;;
;; dset-add
;;
(define/ds (dset-add [(idset elems del _) ds] elem)
  (define prev-count (hash-count elems))
  (let ([elems (hash-set elems elem #t)])
    (if (eqv? prev-count (hash-count elems))
        ds
        (immutable-dset elems del (cons elem (unsafe-dset-seq ds))))))

;;
;; dset-add!
;;
(define/ds (dset-add! [(mdset elems _ seq) ds] elem)
  (define prev-count (hash-count elems))
  (hash-set! elems elem #t)
  (unless (eqv? prev-count (hash-count elems))
    (unsafe-set-dset-seq! ds (cons elem seq))))

;;
;; dset-remove
;;
(define/ds (dset-remove [(idset elems del seq) ds] elem)
  (let* ([elems (hash-remove elems elem)]
         [del (add1 del)])
    (if (too-fragmented? elems del)
        (immutable-dset elems 0 (filter (in? elems) seq))
        (immutable-dset elems del seq))))
;;
;; dset-remove!
;;
(define/ds (dset-remove! [(mdset elems del _) ds] elem)
  (hash-remove! elems elem)
  (let ([del (add1 del)])
    (unsafe-set-dset-del! ds del)
    (when (too-fragmented? elems del)
      (unsafe-set-dset-seq! ds (filter (in? elems) (unsafe-dset-seq ds))))))

;;
;; dset-member?
;;
(define/ds (dset-member? [(dset elems _ _) ds] elem)
  (hash-ref elems elem #f))

;;
;; dset-copy
;;
(define/ds (dset-copy [(mdset elems del seq) ds])
  (let ([elems (hash-copy elems)])
    (mutable-dset elems
                  0
                  (if (zero? del)
                      seq
                      (filter (in? elems) seq)))))

;;
;; dset-empty?
;;
(define/ds (dset-empty? [(dset elems _ _) ds])
  (hash-empty? elems))

;;
;; dset-count
;;
(define/ds (dset-count [(dset elems _ _) ds])
  (hash-count elems))

;;
;; dset-compact!
;;
(define/ds (dset-compact! [(dset _ del _) ds])
  (unless (zero? del)
    (define elems (unsafe-dset-elems ds))
    (unsafe-set-dset-seq! ds (filter (in? elems) (unsafe-dset-seq ds)))
    (unsafe-set-dset-del! ds 0)))

;;
;; dset-compact?
;;
(define/ds (dset-compact? [(dset _ del _) ds])
  (zero? del))

;;
;; dset->list
;;
(define/ds (dset->list [(dset _ del _) ds])
  (cond
    [(zero? del) (unsafe-dset-seq ds)]
    [else (dset-compact! ds)
          (dset->list ds)]))

;;
;; dset-map
;;
(define/ds (dset-map [(dset _ del _) ds] f)
  (cond
    [(zero? del) (map f (unsafe-dset-seq ds))]
    [else (dset-compact! ds)
          (dset-map ds f)]))

;;
;; dset-for-each
;;
(define/ds (dset-for-each [(dset _ del _) ds] f)
  (cond
    [(zero? del) (for-each f (unsafe-dset-seq ds))]
    [else (dset-compact! ds)
          (dset-for-each ds f)]))

;;
;; dset-first
;;
(define/ds (dset-first [(dset elems del seq) ds])
  (cond
    [(zero? del)
     (cond
       [(pair? seq) (car seq)]
       [else (raise-argument-error 'dset-first "a non-empty dset" ds)])]
    [else (dset-compact! ds)
          (dset-first ds)]))

;;
;; dset-rest
;;
(define/ds (dset-rest [(idset elems del seq) ds])
  (cond
    [(zero? del)
     (cond
       [(pair? seq) (immutable-dset (hash-remove elems (car seq)) 0 (cdr seq))]
       [else (raise-argument-error 'dset-rest "a non-empty dset" ds)])]
    [else (dset-compact! ds)
          (dset-rest ds)]))


;;
;; dset-union
;;
;; ds1 : immutable-dset?
;; ds2 : dset? or list?
;; returns an immutable-dset?
(define (dset-union ds1 ds2)
  (unless (immutable-dset? ds1)
    (raise-argument-error 'dset-union "immutable-dset?" ds1))
  (unsafe-dset-union-helper 'dset-union ds1 ds2))


;; unsafe-dset-union-helper
;; ds1 : immutable-dset?
;; ds2 : dset? or list?
;; returns an immutable-dset?
(define (unsafe-dset-union-helper name ds1 ds2)
  (cond
    [(immutable-dset? ds2)
     (cond
       [(> (hash-count (unsafe-dset-elems ds1))
           (hash-count (unsafe-dset-elems ds2)))
        (dset-compact! ds2)
        (unsafe-dset-union-add-list ds1 (unsafe-dset-seq ds2))]
       [else
        (dset-compact! ds1)
        (unsafe-dset-union-add-list ds2 (unsafe-dset-seq ds1))])]
    [(mutable-dset? ds2)
     (dset-compact! ds2)
     (unsafe-dset-union-add-list ds1 (unsafe-dset-seq ds1))]
    [(list? ds2) (unsafe-dset-union-add-list ds1 ds2)]
    [else (raise-argument-error name "(or/c dset? list?)" ds2)]))

;; unsafe-dset-union-add-list
;; dest : immutable-dset?
;; other : list of elements to add
;; returns an immutable-dset?
(define (unsafe-dset-union-add-list dest elems-to-add)
  (define-values (elems _ seq)
    (for*/fold ([elems (unsafe-dset-elems dest)]
                [count (hash-count (unsafe-dset-elems dest))]
                [seq (unsafe-dset-seq dest)])
               ([elem (in-list elems-to-add)]
                [elems (in-value (hash-set elems elem #t))])
      (if (eqv? (hash-count elems) count)
          (values elems count seq)
          (values elems (add1 count) (cons elem seq)))))
  (immutable-dset elems (unsafe-dset-del dest) seq))

;;
;; dset-union*
;;
;; ds : immutable-dset?
;; dss : (listof (or/c dset? list?))
(define (dset-union* ds . dss)
  (unless (immutable-dset? ds)
    (raise-argument-error 'dset-union* "immutable-dset?" ds))
  (for/fold ([ds ds])
            ([new-ds (in-list dss)])
    (unsafe-dset-union-helper 'dset-union* ds new-ds)))

;;
;; dset-union!
;;
;; ds1 : mutable-dset?
;; ds2 : dset? or list?
;; returns void?
(define (dset-union! ds1 ds2)
  (unless (mutable-dset? ds1)
    (raise-argument-error 'dset-union "mutable-dset?" ds1))
  (unsafe-dset-union!-helper 'dset-union! ds1 ds2))

;; unsafe-dset-union!-helper
;; ds1 : mutable-dset?
;; ds2 : dset? or list?
;; returns void?
(define (unsafe-dset-union!-helper name ds1 ds2)
  (cond
    [(dset? ds2)
     (dset-compact! ds2)
     (unsafe-dset-union!-add-list ds1 (unsafe-dset-seq ds2))]
    [(list? ds2) (unsafe-dset-union!-add-list ds1 ds2)]
    [else (raise-argument-error name "(or/c dset? list?)" ds2)]))

;; unsafe-dset-union!-add-list
;; dest : mutable-dset?
;; other : list of elements to add
;; returns void?
(define (unsafe-dset-union!-add-list dest elems-to-add)
  (define elems (unsafe-dset-elems dest))
  (define-values (_ seq)
    (for*/fold ([count (hash-count elems)]
                [seq (unsafe-dset-seq dest)])
               ([elem (in-list elems-to-add)])
      (hash-set! elems elem #t)
      (if (eqv? (hash-count elems) count)
          (values count seq)
          (values (add1 count) (cons elem seq)))))
  (unsafe-set-dset-seq! dest seq))

;;
;; dset-union*!
;;
;; ds : mutable-dset?
;; dss : (listof (or/c dset? list?))
(define (dset-union*! ds . dss)
  (unless (mutable-dset? ds)
    (raise-argument-error 'dset-union*! "mutable-dset?" ds))
  (for ([new-ds (in-list dss)])
    (unsafe-dset-union!-helper 'dset-union*! ds new-ds)))




;;
;; dset-intersect
;;
;; ds1 : immutable-dset?
;; ds2 : dset? or list?
;; returns an immutable-dset?
(define (dset-intersect ds1 ds2)
  (unless (immutable-dset? ds1)
    (raise-argument-error 'dset-intersect "immutable-dset?" ds1))
  (unsafe-dset-intersect-helper 'dset-intersect ds1 ds2))


;; unsafe-dset-intersect-helper
;; ds1 : immutable-dset?
;; ds2 : dset? or list?
;; returns an immutable-dset?
(define (unsafe-dset-intersect-helper name ds1 ds2)
  (cond
    [(immutable-dset? ds2)
     (cond
       [(< (hash-count (unsafe-dset-elems ds1))
           (hash-count (unsafe-dset-elems ds2)))
        (unsafe-dset-intersect ds1 ds2)]
       [else
        (unsafe-dset-intersect ds2 ds1)])]
    [(mutable-dset? ds2)
     (unsafe-dset-intersect ds1 ds2)]
    [(list? ds2) (unsafe-dset-intersect-with-list ds1 ds2)]
    [else (raise-argument-error name "(or/c dset? list?)" ds2)]))


;; unsafe-dset-intersect
;; ds1 : immutable-dset?
;; ds2 : dset?
;; (if both are immutable we favor the smaller one for
;;  ds1 to perform less work!)
(define (unsafe-dset-intersect ds1 ds2)
  (define seq (unsafe-dset-seq ds1))
  (define other-elems (unsafe-dset-elems ds2))
  (define new-elems
    (for/fold ([elems (unsafe-dset-elems ds1)])
              ([elem (in-list seq)])
      (if (hash-ref other-elems elem #f)
          elems
          (hash-remove elems elem))))
  (immutable-dset new-elems 0 (filter (in? new-elems) seq)))

;; unsafe-dset-intersect-with-list
;; ds : immutable-dset?
;; l : list?
(define (unsafe-dset-intersect-with-list ds l)
  (define elems (unsafe-dset-elems ds))
  (define new-elems
    (for/hash ([elem (in-list l)]
               #:when (hash-ref elems elem #f))
      (values elem #t)))
  (immutable-dset new-elems 0 (filter (in? new-elems) (unsafe-dset-seq ds))))


;;
;; dset-intersect*
;;
;; ds : immutable-dset?
;; ds : (listof dset? or list?)
;; returns an immutable-dset?
(define (dset-intersect* ds . dss)
  (unless (immutable-dset? ds)
    (raise-argument-error 'dset-intersect* "immutable-dset?" ds))
  (for/fold ([ds ds])
            ([other (in-list dss)])
    (unsafe-dset-intersect-helper 'dset-intersect* ds other)))



;;
;; dset-intersect!
;;
;; ds1 : mutable-dset?
;; ds2 : dset? or list?
;; returns an immutable-dset?
(define (dset-intersect! ds1 ds2)
  (unless (mutable-dset? ds1)
    (raise-argument-error 'dset-intersect! "mutable-dset?" ds1))
  (unsafe-dset-intersect!-helper 'dset-intersect! ds1 ds2))

;; unsafe-dset-intersect!-helper
;; ds1 : mutable-dset?
;; ds2 : dset? or list?
;; returns void
(define (unsafe-dset-intersect!-helper name ds1 ds2)
  (cond
    [(dset? ds2)
     (unsafe-dset-intersect! ds1 ds2)]
    [(list? ds2) (unsafe-dset-intersect!-with-list ds1 ds2)]
    [else (raise-argument-error name "(or/c dset? list?)" ds2)]))

;; unsafe-dset-intersect!
;; ds1 : mutable-dset?
;; ds2 : dset?
;; returns void
(define (unsafe-dset-intersect! ds1 ds2)
  (define seq (unsafe-dset-seq ds1))
  (define elems (unsafe-dset-elems ds1))
  (define other-elems (unsafe-dset-elems ds2))
  (for ([elem (in-list seq)])
    (unless (hash-ref other-elems elem #f)
      (hash-remove! elems elem)))
  (unsafe-set-dset-del! ds1 0)
  (unsafe-set-dset-seq! ds1 (filter (in? elems) seq)))

;; unsafe-dset-intersect!-with-list
;; ds1 : mutable-dset?
;; l : list
(define (unsafe-dset-intersect!-with-list ds l)
  (define seq (unsafe-dset-seq ds))
  (define elems (unsafe-dset-elems ds))
  (define other-elems (for/hash ([elem (in-list l)])
                        (values elem #t)))
  (for ([elem (in-list seq)])
    (unless (hash-ref other-elems elem #f)
      (hash-remove! elems elem)))
  (unsafe-set-dset-del! ds 0)
  (unsafe-set-dset-seq! ds (filter (in? elems) seq)))

;;
;; dset-intersect*!
;;
;; ds1 : mutable-dset?
;; dss : (listof dset? or list?)
;; returns void
(define (dset-intersect*! ds dss)
  (unless (mutable-dset? ds)
    (raise-argument-error 'dset-intersect*! "mutable-dset?" ds))
  (for ([other (in-list dss)])
    (unsafe-dset-intersect!-helper 'dset-intersect*! ds other)))

;;
;; next-valid-pos
;;
(define-syntax-rule (next-valid-pos elems seq)
  (and (pair? seq) seq))


;;
;; in-dset-proc
;;
(define (in-dset-proc ds)
  (unless (dset? ds)
    (raise-argument-error 'in-dset "dset?" ds))
  (dset-compact! ds)
  (unsafe-dset-seq ds))

;;
;; in-dset
;;
(define-sequence-syntax in-dset
  (λ () #'in-dset-proc)
  (λ (stx)
    (syntax-case stx ()
      [[(elem) (_ ds-exp)]
       #'[(elem)
          (:do-in
           ;; ([(outer-id ...) outer-expr] ...)
           ([(seq)
             (let ([ds ds-exp])
               (unless (dset? ds)
                 (raise-argument-error 'in-dset "dset?" ds))
               (dset-compact! ds)
               (unsafe-ddict-seq ds))])
           ;; outer-check
           #t
           ;; ([loop-id loop-expr] ...)
           ([pos])
           ;; pos-guard
           (pair? pos)
           ;; ([(inner-id ...) inner-expr] ...)
           ([(elem rst) (values (car pos) (cdr pos))])
           ;; pre-guard
           #t
           ;; post-guard
           #t
           ;; (loop-arg ...)
           (rst))]])))





(define-syntax-rule (define-for-immutable-dset for-name for/derived empty-hash)
  (define-syntax (for-name stx)
    (syntax-case stx ()
      [(_ clauses . defs+exprs)
       (with-syntax ([original stx])
         (syntax/loc stx
           (let-values
               ([(elems seq _)
                 (for/derived original
                   ([elems empty-hash]
                    [seq '()]
                    [count 0])
                   clauses
                   (let* ([elem (let () . defs+exprs)]
                          [elems (hash-set elems elem #t)])
                     (if (eqv? (hash-count elems) count)
                         (values elems seq count)
                         (values elems (cons elem seq) (add1 count)))))])
             (immutable-dset elems 0 seq))))])))



(define-for-immutable-dset for/dset for/fold/derived #hash())
(define-for-immutable-dset for/dseteqv for/fold/derived #hasheqv())
(define-for-immutable-dset for/dseteq for/fold/derived #hasheq())
(define-for-immutable-dset for*/dset for*/fold/derived #hash())
(define-for-immutable-dset for*/dseteqv for*/fold/derived #hasheqv())
(define-for-immutable-dset for*/dseteq for*/fold/derived #hasheq())



(define-syntax-rule (define-for-mutable-dset for-name for/derived make-empty-hash)
  (define-syntax (for-name stx)
    (syntax-case stx ()
      [(_ clauses . defs+exprs)
       (with-syntax ([original stx])
         (syntax/loc stx
           (let*-values
               ([(elems) (make-empty-hash)]
                [(seq _)
                 (for/derived original
                   ([seq '()]
                    [count 0])
                   clauses
                   (let ([elem (let () . defs+exprs)])
                     (hash-set! elems elem #t)
                     (if (eqv? (hash-count elems) count)
                         (values seq count)
                         (values (cons elem seq) (add1 count)))))])
             (mutable-dset elems 0 seq))))])))



(define-for-mutable-dset for/mutable-dset for/fold/derived make-hash)
(define-for-mutable-dset for/mutable-dseteqv for/fold/derived make-hasheqv)
(define-for-mutable-dset for/mutable-dseteq for/fold/derived make-hasheq)
(define-for-mutable-dset for*/mutable-dset for*/fold/derived make-hash)
(define-for-mutable-dset for*/mutable-dseteqv for*/fold/derived make-hasheqv)
(define-for-mutable-dset for*/mutable-dseteq for*/fold/derived make-hasheq)




