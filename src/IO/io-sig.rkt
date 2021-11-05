#lang typed/racket/base

(require typed/racket/unit
         "../types.rkt")

(provide io^)


(define-signature io^
  (
   [write-JSON : [->* (JSON)
                      (Output-Port Symbol #:encode Encode)
                      Void]]
   [read-JSON  : (case-> [->* (#:mutable? False)
                              (Input-Port Symbol)
                              (U EOF Immutable-JSON)]
                         [->* (#:mutable? True)
                              (Input-Port Symbol)
                              (U EOF Mutable-JSON)])]

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
                         #:mhash? Boolean)
                        JSExpr]]
   ))
