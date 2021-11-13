#lang racket/base

(require racket/unit
         "../types.rkt"
         "../IO/io-sig.rkt"
         "format-sig.rkt")

(provide format@)


(define-unit format@
  (import io^)
  (export format^)

  (define (format-json js
                       #:type type
                       #:encode [enc 'control]
                       #:indent [indent  "    "])
    (define who 'format-json)

    (define-values (i o) (make-pipe))
    (cond [(string? js) (write-string js o)]
          [(bytes?  js) (write-bytes  js o)])
    (write eof o)
    (define json (read-JSON* who i #:mutable? #f))

    (cond [(eq? type 'string)
           (define o (open-output-string))
           (write-JSON* who json o
                        #:encode  enc
                        #:format? #t
                        #:indent  indent)
           (get-output-string o)]
          [(eq? type 'bytes)
           (define o (open-output-bytes))
           (write-JSON* who json o
                        #:encode  enc
                        #:format? #t
                        #:indent  indent)
           (get-output-bytes o)])))
