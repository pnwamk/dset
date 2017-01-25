#lang racket/base

(require rackunit
         racket/set
         racket/stream
         "../../data/dset.rkt")

;; ----------------------------------------

(check-true (generic-set? (dset)))
(check-true (dset? (dset)))
(check-true (immutable-dset? (dset)))
(check-true (set-empty? (dset)))
(check-true (generic-set? (dset 1 2 3)))
(check-false (set-empty? (dset 1 2 3)))
(check-true (dset? (dset 1 2 3)))
(check-true (immutable-dset? (dset 1 2 3)))
(check-true (generic-set? (dseteq)))
(check-true (set-empty? (dseteq)))
(check-true (generic-set? (dseteq 1 2 3)))
(check-true (dset? (dseteq 1 2 3)))
(check-true (immutable-dset? (dseteq 1 2 3)))
(check-false (set-empty? (dseteq 1 2 3)))
(check-true (generic-set? (dseteqv)))
(check-true (set-empty? (dseteqv)))
(check-true (generic-set? (dseteqv 1 2 3)))
(check-true (immutable-dset? (dseteqv 1 2 3)))
(check-true (dset? (dseteqv 1 2 3)))
(check-false (set-empty? (dseteqv 1 2 3)))
(check-true (generic-set? (mutable-dset)))
(check-true (mutable-dset? (mutable-dset)))
(check-true (dset? (mutable-dset)))
(check-true (set-empty? (mutable-dset)))
(check-true (generic-set? (mutable-dset 1 2 3)))
(check-false (set-empty? (mutable-dset 1 2 3)))
(check-true (mutable-dset? (mutable-dset 1 2 3)))
(check-true (dset? (mutable-dset 1 2 3)))
(check-true (generic-set? (mutable-dseteq)))
(check-true (set-empty? (mutable-dseteq)))
(check-true (generic-set? (mutable-dseteq 1 2 3)))
(check-false (set-empty? (mutable-dseteq 1 2 3)))
(check-true (mutable-dset? (mutable-dseteq 1 2 3)))
(check-true (dset? (mutable-dseteq 1 2 3)))
(check-true (mutable-dset? (mutable-dseteq 1 2 3)))
(check-true (generic-set? (mutable-dseteqv)))
(check-true (set-empty? (mutable-dseteqv)))
(check-true (generic-set? (mutable-dseteqv 1 2 3)))
(check-false (set-empty? (mutable-dseteqv 1 2 3)))
(check-true (mutable-dset? (mutable-dseteqv 1 2 3)))
(check-true (dset? (mutable-dseteqv 1 2 3)))

(check-false (dset-eq? (dset 1 2 3)))
(check-false (dset-eqv? (dset 1 2 3)))
(check-true (dset-equal? (dset 1 2 3)))
(check-true (dset-eq? (dseteq 1 2 3)))
(check-false (dset-eqv? (dseteq 1 2 3)))
(check-false (dset-equal? (dseteq 1 2 3)))
(check-false (dset-eq? (dseteqv 1 2 3)))
(check-true (dset-eqv? (dseteqv 1 2 3)))
(check-false (dset-equal? (dseteqv 1 2 3)))
(check-false (dset-eq? (mutable-dset 1 2 3)))
(check-false (dset-eqv? (mutable-dset 1 2 3)))
(check-true (dset-equal? (mutable-dset 1 2 3)))
(check-true (dset-eq? (mutable-dseteq 1 2 3)))
(check-false (dset-eqv? (mutable-dseteq 1 2 3)))
(check-false (dset-equal? (mutable-dseteq 1 2 3)))
(check-false (dset-eq? (mutable-dseteqv 1 2 3)))
(check-true (dset-eqv? (mutable-dseteqv 1 2 3)))
(check-false (dset-equal? (mutable-dseteqv 1 2 3)))


(check-equal? (set-count (dset (string #\a) "b" "c" (string #\a))) 3)
(check-equal? (set-count (dseteqv (string #\a) "b" "c" (string #\a))) 4)
(check-equal? (set-count (dseteq (string #\a) "b" "c" (string #\a))) 4)
(check-equal? (set-count (mutable-dset (string #\a) "b" "c" (string #\a))) 3)
(check-equal? (set-count (mutable-dseteqv (string #\a) "b" "c" (string #\a))) 4)
(check-equal? (set-count (mutable-dseteq (string #\a) "b" "c" (string #\a))) 4)


(check-true (set-member? (dset 1 2 3) 1))
(check-true (set-member? (dset 1 2 3) 2))
(check-true (set-member? (dset 1 2 3) 3))
(check-false (set-member? (dset 1 2 3) 4))

(check-true (set-member? (dseteq 1 2 3) 1))
(check-true (set-member? (dseteq 1 2 3) 2))
(check-true (set-member? (dseteq 1 2 3) 3))
(check-false (set-member? (dseteq 1 2 3) 4))

(check-true (set-member? (dseteqv 1 2 3) 1))
(check-true (set-member? (dseteqv 1 2 3) 2))
(check-true (set-member? (dseteqv 1 2 3) 3))
(check-false (set-member? (dseteqv 1 2 3) 4))

(check-true (set-member? (mutable-dset 1 2 3) 1))
(check-true (set-member? (mutable-dset 1 2 3) 2))
(check-true (set-member? (mutable-dset 1 2 3) 3))
(check-false (set-member? (mutable-dset 1 2 3) 4))

(check-true (set-member? (mutable-dseteq 1 2 3) 1))
(check-true (set-member? (mutable-dseteq 1 2 3) 2))
(check-true (set-member? (mutable-dseteq 1 2 3) 3))
(check-false (set-member? (mutable-dseteq 1 2 3) 4))

(check-true (set-member? (mutable-dseteqv 1 2 3) 1))
(check-true (set-member? (mutable-dseteqv 1 2 3) 2))
(check-true (set-member? (mutable-dseteqv 1 2 3) 3))
(check-false (set-member? (mutable-dseteqv 1 2 3) 4))


(check-true (stream? (dset 1 2 3)))
(check-equal? (set-first (dset 1 2 3)) (set-first (dset 1 2 3)))
(check-equal? (set-rest (dset 1 2 3))
              (set-remove (dset 1 2 3) (set-first (dset 1 2 3))))

(check-true (stream? (dseteq 1 2 3)))
(check-equal? (set-first (dseteq 1 2 3)) (set-first (dseteq 1 2 3)))
(check-equal? (set-rest (dseteq 1 2 3)) (set-remove (dseteq 1 2 3) (set-first (dseteq 1 2 3))))

(check-true (stream? (dseteqv 1 2 3)))
(check-equal? (set-first (dseteqv 1 2 3)) (set-first (dseteqv 1 2 3)))
(check-equal? (set-rest (dseteqv 1 2 3)) (set-remove (dseteqv 1 2 3) (set-first (dseteqv 1 2 3))))

(check-false (stream? (mutable-dset 1 2 3)))
(check-equal? (set-first (mutable-dset 1 2 3)) (set-first (mutable-dset 1 2 3)))

(check-false (stream? (mutable-dseteq 1 2 3)))
(check-equal? (set-first (mutable-dseteq 1 2 3)) (set-first (mutable-dseteq 1 2 3)))

(check-false (stream? (mutable-dseteqv 1 2 3)))
(check-equal? (set-first (mutable-dseteqv 1 2 3)) (set-first (mutable-dseteqv 1 2 3)))


(check-equal? (dset) (let ([a (dset 1 2 3)]) (set-symmetric-difference a a)))
(check-equal? (dset) (let ([a (dset 1 2 3)] [b (dset 1 2 3)]) (set-symmetric-difference a b)))

(let ([s (dset 1 2 3)])
  (check-equal? s (set-add (set-add (set-add (dset) 1) 2) 3))
  (check-equal? (dseteq 1 2 3) (dseteq 1 2 3))
  (check-equal? (dseteq 1 2 3) (dseteq 3 2 1))
  (check-equal? (dseteqv 1 2 3) (dseteqv 1 2 3))
  (check-not-equal? s (dseteq 1 2 3))
  (check-not-equal? s (dseteqv 1 2 3))
  (check-not-equal? (dseteq 1 2 3) (dseteqv 1 2 3))

  (check-equal? (set->list s) '(3 2 1))
  (check-true (set-member? (set-add s 5) 3))
  (check-true (set-member? (set-add s 5) 5))
  (check-false (set-member? (set-add s 5) 4))
  (check-equal? (set->list (set-add s 5)) '(5 3 2 1))
  
  (check-true (set-member? (set-remove s 5) 3))
  (check-false (set-member? (set-remove s 3) 3))
  (check-equal? (set->list (set-remove s 3))
                '(2 1))

  (check-true (dset-compact? s))
  (check-false (dset-compact? (set-remove s 1)))
  (check-true (dset-compact? (set-remove (set-remove s 1) 2)))
  (check-true (dset-compact? (dset-compact (set-remove s 1))))
  
  (check-true (subset? (dset 1 3) s))
  (check-true (subset? (dset 1 2 3) s))
  (check-false (subset? (dset 1 4) s))
  (check-true (subset? (dset) s))

  (check-equal? (set-count (set-union s)) 3)
  (check-equal? (set-count (set-union s (dset 3 4 5 6))) 6)
  (check-equal? (set-count (set-union (dset 3 4 5 6) s)) 6)
  (check-equal? (set-count (set-union (dset 3 4 5 6) s (dset 1 10 100))) 8)
  (check-equal? (set->list (set-union (dset 1 2 3) (dset 4 5 6)))
                ;; first set is what things are added to since they're the same size
                '(4 5 6 3 2 1))
  (check-equal? (set->list (set-union (dset 1 2 3) (dset 4 5 6 7)))
                ;; second set is what things are added to since it's larger
                '(1 2 3 7 6 5 4))

  (check-equal? (set-union (dseteq 1 2) (dseteq 3)) (dseteq 1 2 3))
  (check-equal? (set-union (dseteqv 1 2) (dseteqv 3)) (dseteqv 1 2 3))

  (check-equal? (set-intersect s) s)
  (check-equal? (set-intersect s (dset 5 4 3 6)) (dset 3))
  (check-false (dset-compact? (set-intersect s (dset 1 2))))
  (check-true (dset-compact? (set-intersect s (dset 5 4 3 6))))
  (check-equal? (set-intersect (dset 5 4 3 6) s) (dset 3))
  (check-equal? (set-intersect (dseteq 5 4 3 6) (dseteq 1 2 3)) (dseteq 3))
  (check-equal? (set-intersect (dseteqv 5 4 3 6) (dseteqv 1 2 3)) (dseteqv 3))
  (check-equal? (set-intersect s (dset 5 2 3)) (dset 3 2))
  (check-equal? (set-intersect (dseteq 1 2 3) (dseteq 5 2 3)) (dseteq 3 2))
  (check-equal? (set-intersect s (dset 5 2 3) (dset 2 20 200)) (dset 2))
  (check-equal? (set-intersect (dseteq 1 2 3) (dseteq 5 2 3) (dseteq 2 20 200)) (dseteq 2))
  (check-equal? (set->list (set-intersect (dseteq 1 2 3) (dseteq 5 2 3)))
                '(3 2))
  
  (check-equal? (set-subtract s) s)
  (check-equal? (set-subtract s s) (dset))
  (check-equal? (set-subtract s (dset 100)) s)
  (check-equal? (set-subtract s (dset 2 100)) (dset 1 3))
  (check-false  (dset-compact? (set-subtract s (dset 2 100))))
  (check-true   (dset-compact? (set-subtract s (dset 1 2 100))))
  (check-equal? (set-subtract (dseteq 2 100) (dseteq 1 2 3)) (dseteq 100))
  (check-equal? (set-subtract (dseteq 2 100 1000 9) (dseteq 1 2 3) (dseteq 1000 5)) (dseteq 9 100))
  (check-equal? (set->list (set-subtract s (dset 2 100)))
                '(3 1))

  (let ([try-mismatch (lambda (set-op)
                        (check-exn #rx"given dsets do not use the same key comparison"
                                   (λ () (set-op (dseteqv 1 2) (dset 3))))
                        (check-exn #rx"given dsets do not use the same key comparison"
                                   (λ () (set-op (dseteq 1 2) (dset 3))))
                        (check-exn #rx"given dsets do not use the same key comparison"
                                   (λ () (set-op (dset 1 2) (dseteq 3))))
                        (check-exn #rx"given dsets do not use the same key comparison"
                                   (λ () (set-op (dset 1 2) (dset 4) (dseteq 3))))
                        (check-exn #rx"given dsets do not use the same key comparison"
                                   (λ () (set-op (dset 1 2) (dseteq 3) (dset 4))))
                        (check-exn #rx"given dsets do not use the same key comparison"
                                   (λ () (set-op (dseteq 3) (dset 1 2) (dset 4)))))])
    (try-mismatch set-union)
    (try-mismatch set-intersect)
    (try-mismatch set-subtract))

  (check-true (andmap negative? (set-map s -)))
  (check-equal? (length (set-map s +)) 3)

  (let ([v 0])
    (set-for-each s (lambda (n) (set! v (+ v n))))
    (check-equal? (values v) 6))

  (check-equal? (sort (for/list ([v s]) v) <) '(1 2 3))
  (check-equal? (sort (for/list ([v (in-set s)]) v) <) '(1 2 3))
  (check-equal? (sort (for/list ([v (in-dset s)]) v) <) '(1 2 3))
  (check-equal? (sort (let ([seq (in-set s)]) (for/list ([v seq]) v)) <) '(1 2 3))
  (check-equal? (sort (let ([seq (in-dset s)]) (for/list ([v seq]) v)) <) '(1 2 3))
  ;; Optimized
  (check-equal? (sort (for/list ([v (in-set (dset 1))]) v) <) '(1))
  (check-true (let ([noset #t])
                (for ([v (in-set (dset))]) (set! noset #f))
                noset))
  (check-equal? (sort (for/list ([v (in-dset (dset 1))]) v) <) '(1))
  (check-true (let ([noset #t])
                (for ([v (in-dset (dset))]) (set! noset #f))
                noset)))


(let ([s (mutable-dset 1 2 3)]
      [other-s (mutable-dset)])

  (check-equal? (set->list s) '(3 2 1))
  
  (set-add! other-s 1)
  (set-add! other-s 2)
  (set-add! other-s 3)
  (check-equal? s other-s)

  (set! s (mutable-dset))
  
  (check-equal? (mutable-dseteq 1 2 3)
                (mutable-dseteq 1 2 3))
  (check-equal? (mutable-dseteq 1 2 3)
                (mutable-dseteq 3 2 1))
  (check-equal? (mutable-dseteqv 1 2 3)
                (mutable-dseteqv 1 2 3))
  (check-not-equal? (mutable-dset 1 2 3)
                    (mutable-dseteq 1 2 3))
  (check-not-equal? (mutable-dset 1 2 3)
                    (mutable-dseteqv 1 2 3))
  (check-not-equal? (mutable-dseteq 1 2 3)
                    (mutable-dseteqv 1 2 3))

  (set! s (mutable-dset 1 2 3))
  (set-add! s 5)
  (check-true (set-member? s 3))
  (check-true (set-member? s 5))
  (check-false (set-member? s 4))
  (check-equal? (set->list s) '(5 3 2 1))
  

  (set-remove! s 5)
  (check-true (set-member? s 3))
  (set-remove! s 3)
  (check-false (set-member? s 3))
  (check-equal? (set->list s) '(2 1))


  (set! s (mutable-dset 1 2 3))
  (check-true (dset-compact? s))
  (set-remove! s 1)
  (check-false (dset-compact? s))
  (set-remove! s 2)
  (check-true (dset-compact? s))
  (set! s (mutable-dset 1 2 3))
  (set-remove! s 1)
  (dset-compact! s)
  (check-true (dset-compact? s))


  
  (set! s (mutable-dset 1 2 3))

  (check-true (subset? (mutable-dset 1 3) s))
  (check-true (subset? (mutable-dset 1 2 3) s))
  (check-false (subset? (mutable-dset 1 4) s))
  (check-true (subset? (mutable-dset) s))

  (set-union! s)
  (check-equal? (set-count s) 3)

  (set! s (mutable-dset 1 2 3))
  (set-union! s (mutable-dset 3 4 5 6))
  (check-equal? (set-count s) 6)
  (check-equal? (set->list s) '(4 5 6 3 2 1))

  (set! s (mutable-dset 1 2 3))
  (set-union! s (mutable-dset 3 4 5 6))
  (check-equal? (set-count s) 6)
  
  (set! s (mutable-dset 1 2 3))
  (set-union! s (mutable-dset 3 4 5 6) (mutable-dset 1 7 8))
  (check-equal? (set-count s) 8)

  
  (set! s (mutable-dseteq 1 2))
  (set-union! s (mutable-dseteq 3))
  (check-equal? s (mutable-dseteq 1 2 3))


  (set! s (mutable-dseteqv 1 2))
  (set-union! s (mutable-dseteqv 3))
  (check-equal? s (mutable-dseteqv 1 2 3))

  (set! s (mutable-dset 1 2 3))
  (set-intersect! s)
  (check-equal? s (mutable-dset 1 2 3))

  (set! s (mutable-dset 1 2 3))
  (set-intersect! s (mutable-dset 5 4 3 6 2))
  (check-false (dset-compact? s))
  (check-equal? s (mutable-dset 3 2))
  (check-equal? (set->list s) '(3 2))
  (set-intersect! s (mutable-dset 2))
  (check-true (dset-compact? s))
  (check-equal? (set->list s) '(2))
  

  (set! s (mutable-dset 5 4 3 6))
  (set-intersect! s (mutable-dset 1 2 3))
  (check-equal? s (mutable-dset 3))

  (set! s (mutable-dseteq 1 2 3))
  (set-intersect! s (mutable-dseteq 2 3 4) (mutable-dseteq 3 4 5))
  (check-equal? s (mutable-dseteq 3))

  (set! s (mutable-dseteqv 1 2 3))
  (set-intersect! s (mutable-dseteqv 5 4 3 6))
  (check-equal? s (mutable-dseteqv 3))

  (set! s (mutable-dset 1 2 3))
  (set-intersect! s (mutable-dset 5 2 3))
  (check-equal? s (mutable-dset 3 2))

  (set! s (mutable-dseteq 1 2 3))
  (set-intersect! s (mutable-dseteq 5 2 3))
  (check-equal? s (mutable-dseteq 3 2))

  (set! s (mutable-dset 1 2 3))
  (set-intersect! s
                  (mutable-dset 5 2 3)
                  (mutable-dset 2 20 200))
  (check-equal? s (mutable-dset 2))

  (set! s (mutable-dseteq 1 2 3))
  (set-intersect! s
                  (mutable-dseteq 5 2 3)
                  (mutable-dseteq 2 20 200))
  (check-equal? s (mutable-dseteq 2))



  (set! s (mutable-dset 1 2 3))
  (set-subtract! s)
  (check-equal? s (mutable-dset 1 2 3))

  (set-subtract! s s)
  (check-equal? s (mutable-dset))

  (set! s (mutable-dset 1 2 3))
  (set-subtract! s (mutable-dset 100))
  (check-equal? s (mutable-dset 1 2 3))

  (set! s (mutable-dset 1 2 3))
  (set-subtract! s (mutable-dset 2 100))
  (check-false (dset-compact? s))
  (check-equal? s (mutable-dset 1 3))
  (check-equal? (set->list s) '(3 1))
  (set-subtract! s (mutable-dset 3))
  (check-true (dset-compact? s))
  (check-equal? s (mutable-dset 1))
  (check-equal? (set->list s) '(1))
  
  (set! s (mutable-dseteq 2 100))
  (set-subtract! s (dseteq 1 2 3))
  (check-equal? s (mutable-dseteq 100))

  (set! s (mutable-dseteq 2 100 1000 9))
  (set-subtract! s (dseteq 1 2 3) (dseteq 1000 5))
  (check-equal? s (mutable-dseteq 9 100))

  (let ([try-mismatch (lambda (set-op!)
                        (check-exn #rx"given dsets do not use the same key comparison"
                                   (λ () (set-op! (mutable-dseteqv 1 2)
                                                  (mutable-dset 3))))
                        (check-exn #rx"given dsets do not use the same key comparison"
                                   (λ () (set-op! (mutable-dseteq 1 2)
                                                  (mutable-dset 3))))
                        (check-exn #rx"given dsets do not use the same key comparison"
                                   (λ () (set-op! (mutable-dset 1 2)
                                                  (mutable-dseteq 3))))
                        (check-exn #rx"given dsets do not use the same key comparison"
                                   (λ () (set-op! (mutable-dset 1 2)
                                                  (mutable-dset 4)
                                                  (mutable-dseteq 3))))
                        (check-exn #rx"given dsets do not use the same key comparison"
                                   (λ () (set-op! (mutable-dset 1 2)
                                                  (mutable-dseteqv 3)
                                                  (mutable-dset 4))))
                        (check-exn #rx"given dsets do not use the same key comparison"
                                   (λ () (set-op! (mutable-dseteq 3)
                                                  (mutable-dset 1 2)
                                                  (mutable-dset 4)))))])
    (try-mismatch set-union!)
    (try-mismatch set-intersect!)
    (try-mismatch set-subtract!))

  
  (set! s (mutable-dset 1 2 3))
  
  (check-true (andmap negative? (set-map s -)))
  (check-equal? (length (set-map s +)) 3)

  (let ([v 0])
    (set-for-each s (lambda (n) (set! v (+ v n))))
    (check-equal? v 6))

  
  (check-equal? (for/list ([v (in-set s)]) v) '(3 2 1))
  (check-equal? (for/list ([v (in-dset s)]) v) '(3 2 1))
  (check-equal? (let ([seq (in-set s)]) (for/list ([v seq]) v)) '(3 2 1))
  (check-equal? (let ([seq (in-dset s)]) (for/list ([v seq]) v)) '(3 2 1))
  (check-equal? (for/list ([v (in-set (mutable-dset 1 2 3))]) v) '(3 2 1))
  (check-equal? (for/list ([v (in-dset (mutable-dset 1 2 3))]) v) '(3 2 1))
  (check-equal? (let ([seq (in-set (mutable-dset 1 2 3))]) (for/list ([v seq]) v)) '(3 2 1))
  (check-equal? (let ([seq (in-dset (mutable-dset 1 2 3))]) (for/list ([v seq]) v)) '(3 2 1))
  ;; Optimized
  (check-equal? (for/list ([v (in-set (dset 1))]) v) '(1))
  (check-equal? (for/list ([v (in-dset (dset 1))]) v) '(1))
  (set! s (mutable-dset))
  (check-true (let ([noset #t])
                (for ([v (in-set (dset))]) (set! noset #f))
                noset))
  (check-true (let ([noset #t])
                (for ([v (in-dset (dset))]) (set! noset #f))
                noset)))



(define (list->some-dset-tests list->ds constructor)
  (check-equal? (list->ds '()) (constructor))
  (check-equal? (list->ds '(1 2 3)) (constructor 1 2 3))
  (check-equal? (set->list (list->ds '(1 2 3))) '(3 2 1))
  (check-equal? (set->list (set-add (list->ds '(1 2 3)) 4)) '(4 3 2 1))
  (check-equal? (set->list (set-remove (set-add (list->ds '(1 2 3)) 4) 3)) '(4 2 1)))

(list->some-dset-tests list->dset dset)
(list->some-dset-tests list->dseteqv dseteqv)
(list->some-dset-tests list->dseteq dseteq)

(define (list->some-mutable-dset-tests list->ds constructor)
  (check-equal? (list->ds '()) (constructor))
  (check-equal? (list->ds '(1 2 3)) (constructor 1 2 3))
  (check-equal? (set->list (list->ds '(1 2 3))) '(3 2 1))
  (define s (list->ds '(1 2 3)))
  (set-add! s 4)
  (check-equal? (set->list s) '(4 3 2 1))
  (set! s (list->ds '(1 2 3)))
  (set-add! s 4)
  (set-remove! s 2)
  (check-equal? (set->list s) '(4 3 1)))

(list->some-mutable-dset-tests list->mutable-dset mutable-dset)
(list->some-mutable-dset-tests list->mutable-dseteqv mutable-dseteqv)
(list->some-mutable-dset-tests list->mutable-dseteq mutable-dseteq)


;; ----------------------------------------

(check-equal? (for/dset ([i '(0 1 2)]) (add1 i)) (dset 1 2 3))

(check-equal? (for/dset ([i '(0 1 2 3 4)])
                #:break (= i 3)
                (add1 i))
              (dset 1 2 3))

(check-equal? (for/dset ([i '(0 1 2 3 4)]) 
                #:final (= i 2)
                (add1 i))
              (dset 1 2 3))

(check-equal? (for/mutable-dset ([i '(0 1 2)]) (add1 i))
              (mutable-dset 1 2 3))

(check-equal? (for/mutable-dset ([i '(0 1 2 3 4)]) 
                               #:break (= i 3)
                               (add1 i))
              (mutable-dset 1 2 3))

(check-equal? (for/mutable-dset ([i '(0 1 2 3 4)]) 
                               #:final (= i 2)
                               (add1 i))
              (mutable-dset 1 2 3))

;; ----------------------------------------
;; ordering + for checks
(check-equal? (set->list (for/dset ([i '(0 1 2)]) (add1 i)))
              '(3 2 1))
(check-equal? (set->list (for/dseteqv ([i '(0 1 2)]) (add1 i)))
              '(3 2 1))
(check-equal? (set->list (for/dseteq ([i '(0 1 2)]) (add1 i)))
              '(3 2 1))
(check-equal? (set->list (for*/dset ([i '(0 1 2)]) (add1 i)))
              '(3 2 1))
(check-equal? (set->list (for*/dseteqv ([i '(0 1 2)]) (add1 i)))
              '(3 2 1))
(check-equal? (set->list (for*/dseteq ([i '(0 1 2)]) (add1 i)))
              '(3 2 1))
(check-equal? (set->list (for/mutable-dset ([i '(0 1 2)]) (add1 i)))
              '(3 2 1))
(check-equal? (set->list (for/mutable-dseteq ([i '(0 1 2)]) (add1 i)))
              '(3 2 1))
(check-equal? (set->list (for/mutable-dseteqv ([i '(0 1 2)]) (add1 i)))
              '(3 2 1))
(check-equal? (set->list (for*/mutable-dset ([i '(0 1 2)]) (add1 i)))
              '(3 2 1))
(check-equal? (set->list (for*/mutable-dseteq ([i '(0 1 2)]) (add1 i)))
              '(3 2 1))
(check-equal? (set->list (for*/mutable-dseteqv ([i '(0 1 2)]) (add1 i)))
              '(3 2 1))


