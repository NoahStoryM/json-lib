#lang typed/racket/base

(require typed/racket/unit
         "../types.rkt")

(provide jsexpr^)


(define-signature jsexpr^
  (
   ;; Predicate
   [jsexpr? : [-> Any
                  [#:null JSExpr]
                  [#:inf+ JSExpr]
                  [#:inf- JSExpr]
                  Boolean]]

   ;; IO
   [write-jsexpr : [->* (JSExpr)
                        (Output-Port
                         Symbol
                         #:null JSExpr
                         #:inf+ JSExpr
                         #:inf- JSExpr
                         #:encode Encode)
                        Void]]
   [read-jsexpr  : [->* ()
                        (Input-Port
                         Symbol
                         #:null JSExpr
                         #:inf+ JSExpr
                         #:inf- JSExpr
                         #:mlist? Boolean
                         #:mhash? Boolean)
                        JSExpr]]

   ;; Conversion
   [jsexpr-copy    : [-> JSExpr
                         [#:null JSExpr]
                         [#:inf+ JSExpr]
                         [#:inf- JSExpr]
                         [#:mlist? Boolean]
                         [#:mhash? Boolean]
                         JSExpr]]
   [json->jsexpr   : [-> JSON
                         [#:null JSExpr]
                         [#:inf+ JSExpr]
                         [#:inf- JSExpr]
                         [#:mlist? Boolean]
                         [#:mhash? Boolean]
                         JSExpr]]

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
                           #:mlist? Boolean
                           #:mhash? Boolean)
                          (U EOF JSExpr)]]
   [bytes->jsexpr  : [->* (Bytes)
                          (Symbol
                           #:null JSExpr
                           #:inf+ JSExpr
                           #:inf- JSExpr
                           #:mlist? Boolean
                           #:mhash? Boolean)
                          (U EOF JSExpr)]]
   ))
