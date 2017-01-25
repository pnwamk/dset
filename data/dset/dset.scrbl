#lang scribble/doc

@(require scribble/manual
          scribble/example
          (for-label racket/base
                     racket/contract
                     data/dset))

@title{Simple, Deterministic Sets}
@author{@(author+email "Andrew Kent" "andmkent@iu.edu")}

@(define (concurrency-caveat)
  @elemref['(caveat "concurrent dset modification")]{caveats concerning concurrent modification})
@(define (mutable-element-caveat)
  @elemref['(caveat "mutating dset elements")]{caveat concerning mutable elements})

@(define (see-also-caveats)
   @t{See also the @concurrency-caveat[] and the @mutable-element-caveat[] above.})
@(define (see-also-concurrency-caveat)
   @t{See also the @concurrency-caveat[] above.})
@(define (see-also-mutable-element-caveat)
@t{See also the @mutable-element-caveat[] above.})


@(define the-eval (make-base-eval))
@(the-eval '(require data/dset))



@defmodule[data/dset]

This package defines immutable and mutable @deftech{
 deterministic sets} (or @deftech{dset}s, for
short). A @tech{dset} is a
@tech[#:doc '(lib "scribblings/reference/reference.scrbl")]{
 generic set} that guarantees LIFO ordering when iterating
over the elements.


@examples[
 #:eval the-eval
 (define ds (dset 0 1 2))
 ds
 "Note the ordering is LIFO w.r.t. insertion order"
 (dset->list (dset-add ds 42))
 
 (define mds (for/mutable-dset ([n (in-list '(null eins zwei drei))])
               n))
 mds
 (set-add! mds 'vier)
 (set-add! mds 'sechs)
 (set-remove! mds 'eins)
 (set-remove! mds 'drei)
 (in-set mds)
 (for ([elem (in-dset mds)])
   (printf "element ~a: ~a\n" n elem))
 ]



@elemtag['(caveat "concurrent dset modification")]{@bold{
  Caveats concerning concurrent modification:}} A mutable dset
can be manipulated with functions such as @racket[set-add!],
and @racket[set-remove!] concurrently by multiple threads;
updates to internal state are performed with a check-and-set
operation which protects from invalid internal states.
However, elements which are removed concurrently during the
following operations may or may not be visited during the
traversal: @racket[set-map], @racket[set-for-each],
@racket[in-set] and @racket[in-dset].


@elemtag['(caveat "mutating dset elements")]{@bold{Caveat
  concerning mutable elements:}} If an element in an
@racket[equal?]-based dset is mutated, then the dset's
behavior for insertion and lookup operations becomes
unpredictable.



@section{Constructors}


@deftogether[(
  @defproc[(dset [val any/c] ...) (and/c immutable-dset? dset-equal?)]
  @defproc[(dseteqv [val any/c] ...) (and/c immutable-dset? dset-eqv?)]
  @defproc[(dseteq [val any/c] ...) (and/c immutable-dset? dset-eq?)]
  @defproc[(mutable-dset [val any/c] ...) (and/c mutable-dset? dset-equal?)]
  @defproc[(mutable-dseteqv [val any/c] ...) (and/c mutable-dset? dset-eqv?)]
  @defproc[(mutable-dseteq [val any/c] ...) (and/c mutable-dset? dset-eq?)]
)]{

 Creates a @tech{dset} with the given @racket[val]s. Each
 constructor also specifies how elements are compared (e.g.
 @racket[dset] compares elements with @racket[equal?],
 @racket[dseteqv] compares elements with @racket[eqv?], etc).

 The @racket[val]s are added in the order they appear in the
 argument list, so later mappings can hide earlier ones if
 they are equal.}

@deftogether[(
   @defproc[(list->dset [elems list?]) (and/c immutable-dset? dset-equal?)]
   @defproc[(list->dseteqv [elems list?]) (and/c immutable-dset? dset-eqv?)]
   @defproc[(list->dseteq [elems list?]) (and/c immutable-dset? dset-eq?)]
   @defproc[(list->mutable-dset [elems list?]) (and/c mutable-dset? dset-equal?)]
   @defproc[(list->mutable-dseteqv [elems list?]) (and/c mutable-dset? dset-eqv?)]
   @defproc[(list->mutable-dseteq [elems list?]) (and/c mutable-dset? dset-eq?)]
)]{ Creates a @tech{dset} that is initialized with the
 contents of @racket[elems]. The elements are added to the
 set in the order they appear in the argument list, so
 later mappings can hide earlier ones if the elements
 are equivalent w.r.t. the set's comparison function.

}

@section{Basic Predicates}

@defproc[(dset? [v any/c]) boolean?]{
Returns @racket[#t] if @racket[v] is a @tech{dset} (i.e. if it is either
 a @tech{immutable-dset} or a @tech{mutable-dset}), @racket[#f] otherwise.
}

@defproc[(immutable-dset? [v any/c]) boolean?]{
Returns @racket[#t] if @racket[v] is an @deftech{immutable-dset}, @racket[#f] otherwise.
}

@defproc[(mutable-dset? [v any/c]) boolean?]{
Returns @racket[#t] if @racket[v] is a @deftech{mutable-dset}, @racket[#f] otherwise.
}


@deftogether[(
  @defproc[(dset-equal? [dd dset?]) boolean?]
  @defproc[(dset-eqv? [dd dset?]) boolean?]
  @defproc[(dset-eq? [dd dset?]) boolean?]
)]{

 @racket[dset-equal?] returns @racket[#t] if the given @tech{dset}'s elements are compared with @racket[equal?], @racket[#f] otherwise.

 @racket[dset-eqv?] returns @racket[#t] if the given @tech{dset}'s elements are compared with @racket[eqv?], @racket[#f] otherwise.

 @racket[dset-eq?] returns @racket[#t] if the given @tech{dset}'s elements are compared with @racket[eq?], @racket[#f] otherwise.
}

@section{Traversal and Iteration}

@defproc[(in-dset [ds dset?]) sequence?]{
Returns a sequence containing the elements of @racket[ds]
 in LIFO order w.r.t. the order they were inserted into @racket[ds].
}


@deftogether[(
@defform[(for/dset (for-clause ...) body-or-break ... body)]
@defform[(for/dseteqv (for-clause ...) body-or-break ... body)]
@defform[(for/dseteq (for-clause ...) body-or-break ... body)]
@defform[(for*/dset (for-clause ...) body-or-break ... body)]
@defform[(for*/dseteqv (for-clause ...) body-or-break ... body)]
@defform[(for*/dseteq (for-clause ...) body-or-break ... body)]
@defform[(for/mutable-dset (for-clause ...) body-or-break ... body)]
@defform[(for/mutable-dseteqv (for-clause ...) body-or-break ... body)]
@defform[(for/mutable-dseteq (for-clause ...) body-or-break ... body)]
@defform[(for*/mutable-dset (for-clause ...) body-or-break ... body)]
@defform[(for*/mutable-dseteqv (for-clause ...) body-or-break ... body)]
@defform[(for*/mutable-dseteq (for-clause ...) body-or-break ... body)]
)]{
  Like @racket[for/set], but producing a @tech{dset} with the
 respective mutability and element comparison function.
}

@section{Performance and Memory Usage}

@bold{Performance.} Immutable and mutable @tech{dset}s
internally use Racket's immutable @racket[hash] data
structure along with a @racket[list] of the elements in
order to provide @racket[set]-like performance and a
deterministic iteration order. Performing set operations on
@tech{dset}s will have a small overhead in comparison to
Racket's unordered @racket[set]s, but the asymptotic
complexity is unaffected.

@bold{Memory Usage.} In order to keep @tech{dset} operations
such as @racket[dset-remove] efficient (i.e. non-linear), we
do not immediately remove elements from the internal element
list. Instead, a certain amount of ``fragmentation'' in the
element list is tolerated, but once it passes a
predetermined threshold (when the number of removed element
slots exceeds the number of active elements), the list is
then ``defragmented''. To prevent this fragmentation from
causing unexpected memory leaks, each element in the list is
stored in a
@tech[#:doc '(lib "scribblings/reference/reference.scrbl")]{
 weak-box} so its presence in the element list after removal
does not prevent garbage collection that would otherwise
occur.

The hope is that these implementation details are mostly
unobservable to @tech{dset} users since their costs will be
amortized.

If a user is concerned and wants more fine grained control
over the presence of internal fragmentation (e.g. if
removals are performed early on in computation then never
again) the following functions report on the presence
of fragmentation and allow a user to remove any:


@defproc[(dset-compact? [ds dset?]) boolean?]{
Returns @racket[#t] if @racket[ds] contains no fragmentation
        in its element list, otherwise returns @racket[#f].
}

@defproc[(dset-compact [ds immutable-dset?]) immutable-dset?]{
 Returns a defragmented version of @racket[ds] (i.e. the
 elements are the same, but any unnecessary space in the
 internal element list is removed).}

@defproc[(dset-compact! [ds mutable-dset?]) void?]{
 Defragments the internal element list of @racket[ds].}