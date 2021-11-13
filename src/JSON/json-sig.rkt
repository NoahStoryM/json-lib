#lang racket/base

(require racket/unit
         "../types.rkt")

(provide json^)


(define-signature json^
  (
   ;; Predicate
   mjson?
   json-mlist?
   json-mhash?

   ;; IO
   write-JSON
   read-JSON

   ;; Conversion
   json-copy
   jsexpr->json

   json->string
   json->bytes
   string->json
   bytes->json
   ))
