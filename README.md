# Deterministic Sets for Racket

[![Build Status](https://travis-ci.org/pnwamk/dset.svg?branch=master)](https://travis-ci.org/pnwamk/dset)

This library provides a simple set data type that resembles Racket's built in `set` data structure, except that it provides deterministic (LIFO) ordering for iteration and key/value lists.

Functionality is intended to match Racket's built in `set` in the user API and performance as much as possible, so that dset's can be a simple drop-in replacement for set's when determinstic ordering is desired.

## Installation

Preferred method:
```
raco pkg install dset
```

or the alternate method:
```
raco pkg install git://github.com/pnwamk/dset.git
```

## Usage

```racket
(require data/dset)

(define d (dset 'Sunday 'Monday ...))

(dset-member? dd 'Monday) ;; ==> #t
```

# API

The API for these dictionaries is intended to be identical to the `set` API as much as possible. The following identifiers are provided:


WORK IN PROGRESS
