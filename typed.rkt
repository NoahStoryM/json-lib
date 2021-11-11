#lang typed/racket/base

(require "src/main.rkt")
(require/typed "untyped.rkt"
  [mutable-json? [-> Any Boolean]]
  [json?         [-> Any Boolean]])

(provide (all-from-out "src/main.rkt")
         mutable-json?
         json?)
