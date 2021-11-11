#lang typed/racket/base/no-check

(require "src/main.rkt"
         "src/untyped-help.rkt")

(provide (all-from-out "src/main.rkt")
         mutable-json?
         json?)


(: mutable-json? [-> Any Boolean])
(define (mutable-json? x)
  (or (json-constant? x)
      (null? x)
      (and (mlist? x) (andmmap mutable-json? x))
      (and (hash? x) (not (immutable? x))
           (for/and ([(k v) (in-hash x)])
             (and (symbol? k) (mutable-json? v))))))

(: json? [-> Any Boolean])
(define (json? x) (or (immutable-json? x) (mutable-json? x)))
