#lang typed/racket/base

(require typed/racket/unit
         "../types.rkt")

(provide custom^)


(define-signature custom^
  (
   [json-null : (Parameter JSExpr)]
   [json-inf+ : (Parameter JSExpr)]
   [json-inf- : (Parameter JSExpr)]

   [JSON-null : JSON-Null]
   [JSON-inf+ : JSON-Pos-Inf]
   [JSON-inf- : JSON-Neg-Inf]

   [jsexpr-mhash? : (Parameter Boolean)]
   ))
