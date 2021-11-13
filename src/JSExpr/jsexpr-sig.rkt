#lang racket/base

(require racket/unit
         "../types.rkt")

(provide jsexpr^)


(define-signature jsexpr^
  (
   ;; Predicate
   jsexpr?

   ;; IO
   write-jsexpr
   read-jsexpr

   ;; Conversion
   jsexpr-copy
   json->jsexpr

   jsexpr->string
   jsexpr->bytes
   string->jsexpr
   bytes->jsexpr
   ))
