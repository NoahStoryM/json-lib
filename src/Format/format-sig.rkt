#lang typed/racket/base

(require typed/racket/unit)

(provide format^)


(define-signature format^
  (
   [format-json : (case-> [-> (U String Bytes) #:type 'string [#:indent String] String]
                          [-> (U String Bytes) #:type 'bytes  [#:indent String] Bytes ])]
   ))
