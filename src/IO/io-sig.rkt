#lang typed/racket/base

(require typed/racket/unit
         "../types.rkt")

(provide io^)


(define-signature io^
  (
   [write-JSON* : [-> Symbol JSON Output-Port
                      [#:encode  Encode]
                      [#:format? Boolean]
                      [#:indent  String]
                      Void]]
   [read-JSON*  : (case-> [-> Symbol Input-Port #:mutable? False (U EOF Immutable-JSON)]
                          [-> Symbol Input-Port #:mutable? True  (U EOF Mutable-JSON)])]
   ))
