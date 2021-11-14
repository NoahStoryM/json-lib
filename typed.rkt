#lang typed/racket/base

(require typed/racket/unsafe)

(require "src/main.rkt")
(unsafe-require/typed "untyped.rkt"
  [mutable-json? [-> Any Boolean : Mutable-JSON]])

(provide (all-from-out "src/main.rkt")
         mutable-json?
         json?)

(: json? [-> Any Boolean : JSON])
(define (json? arg)
  (or (immutable-json? arg)
      (mutable-json? arg)))
