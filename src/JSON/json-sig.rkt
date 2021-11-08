#lang typed/racket/base

(require typed/racket/unit
         "../types.rkt")

(provide json^)


(define-signature json^
  (
   ;; Predicate
   [mjson?      : [-> JSON Boolean : Mutable-JSON]]
   [json-mlist? : [-> JSON Boolean : JS-MList]]
   [json-mhash? : [-> (U EOF JSON) Boolean : JS-MHash]]

   ;; IO
   [write-JSON : [->* (JSON) (Output-Port Symbol #:encode Encode) Void]]
   [read-JSON  : (case-> [->* (#:mutable? False) (Input-Port Symbol) (U EOF Immutable-JSON)]
                         [->* (#:mutable? True) (Input-Port Symbol) (U EOF Mutable-JSON)])]

   ;; Conversion
   [json-copy    : (case-> [-> JSON #:mutable? False Immutable-JSON]
                           [-> JSON #:mutable? True  Mutable-JSON])]
   [jsexpr->json : (case-> [-> JSExpr
                               #:mutable? False
                               [#:null JSExpr]
                               [#:inf+ JSExpr]
                               [#:inf- JSExpr]
                               Immutable-JSON]
                           [-> JSExpr
                               #:mutable? True
                               [#:null JSExpr]
                               [#:inf+ JSExpr]
                               [#:inf- JSExpr]
                               Mutable-JSON])]

   [json->string : [->* (JSON) (Symbol #:encode Encode) String]]
   [json->bytes  : [->* (JSON) (Symbol #:encode Encode) Bytes]]
   [string->json : (case-> [->* (String #:mutable? False) (Symbol) (U EOF Immutable-JSON)]
                           [->* (String #:mutable? True ) (Symbol) (U EOF Mutable-JSON)])]
   [bytes->json  : (case-> [->* (Bytes #:mutable? False) (Symbol) (U EOF Immutable-JSON)]
                           [->* (Bytes #:mutable? True ) (Symbol) (U EOF Mutable-JSON)])]
   ))
