#lang typed/racket/base

(require typed/racket/unit
         "../types.rkt")

(provide convert^)


(define-signature convert^
  (
   [json->jsexpr   : [-> JSON
                         [#:null JSExpr]
                         [#:inf+ JSExpr]
                         [#:inf- JSExpr]
                         [#:mhash? Boolean]
                         JSExpr]]
   [jsexpr->json   : (case-> [-> JSExpr
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

   [json->string   : [->* (JSON) (Symbol #:encode Encode) String]]
   [json->bytes    : [->* (JSON) (Symbol #:encode Encode) Bytes]]
   [string->json   : (case-> [->* (String #:mutable? False) (Symbol) (U EOF Immutable-JSON)]
                             [->* (String #:mutable? True ) (Symbol) (U EOF Mutable-JSON)])]
   [bytes->json    : (case-> [->* (Bytes #:mutable? False) (Symbol) (U EOF Immutable-JSON)]
                             [->* (Bytes #:mutable? True ) (Symbol) (U EOF Mutable-JSON)])]

   [jsexpr->string : [->* (JSExpr)
                          (Symbol
                           #:null JSExpr
                           #:inf+ JSExpr
                           #:inf- JSExpr
                           #:encode Encode)
                          String]]
   [jsexpr->bytes  : [->* (JSExpr)
                          (Symbol
                           #:null JSExpr
                           #:inf+ JSExpr
                           #:inf- JSExpr
                           #:encode Encode)
                          Bytes]]
   [string->jsexpr : [->* (String)
                          (Symbol
                           #:null JSExpr
                           #:inf+ JSExpr
                           #:inf- JSExpr
                           #:mhash? Boolean)
                          (U EOF JSExpr)]]
   [bytes->jsexpr  : [->* (Bytes)
                          (Symbol
                           #:null JSExpr
                           #:inf+ JSExpr
                           #:inf- JSExpr
                           #:mhash? Boolean)
                          (U EOF JSExpr)]]
   ))
