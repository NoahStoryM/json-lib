#lang typed/racket/base

(require typed/racket/unsafe)

(require "src/main.rkt")
(unsafe-require/typed "untyped.rkt"
  [mutable-json? [-> Any Boolean]]
  [json?         [-> Any Boolean]])

(provide (all-from-out "src/main.rkt")
         mutable-json?
         json?)
