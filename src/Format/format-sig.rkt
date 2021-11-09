#lang typed/racket/base

(require typed/racket/unit
         "../types.rkt")

(provide format^)


(define-signature format^
  (
   [format-json : (case-> [-> (U String Bytes)
                              #:type 'string
                              [#:encode Encode]
                              [#:indent String]
                              String]
                          [-> (U String Bytes)
                              #:type 'bytes
                              [#:encode Encode]
                              [#:indent String]
                              Bytes])]
   ))
