#lang racket/base

(require racket/unit
         "../types.rkt")

(provide custom^)


(define-signature custom^
  (
   json-null
   json-inf+
   json-inf-

   JSON-null
   JSON-inf+
   JSON-inf-

   jsexpr-mhash?
   ))
