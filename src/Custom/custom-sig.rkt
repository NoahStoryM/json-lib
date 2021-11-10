#lang typed/racket/base

(require typed/racket/unit
         "../types.rkt")

(provide custom^)


(define-signature custom^
  (
   [json-null : (Parameter JSExpr)]
   [json-inf+ : (Parameter JSExpr)]
   [json-inf- : (Parameter JSExpr)]

   [JSON-null : JS-Null]
   [JSON-inf+ : JS-Pos-Inf]
   [JSON-inf- : JS-Neg-Inf]

   [jsexpr-mhash? : (Parameter Boolean)]
   ))
