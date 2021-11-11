#lang typed/racket/base/no-check

(require "src/main.rkt"
         "src/untyped-help.rkt"

         racket/contract)

(provide (except-out
          (all-from-out "src/main.rkt")
          ;; Predicate
          mjson?
          json-mlist?
          json-mhash?

          ;; IO
          write-JSON
          read-JSON

          ;; Conversion
          json-copy
          jsexpr->json
          json->jsexpr

          json->string
          json->bytes
          string->json
          bytes->json)

         (contract-out
          ;; Predicate
          [json?         [-> any/c boolean?]]
          [mutable-json? [-> any/c boolean?]]

          [mjson?        [-> json? boolean?]]
          [json-mlist?   [-> json? boolean?]]
          [json-mhash?   [-> (or/c eof-object? json?) boolean?]]

          ;; IO
          [write-JSON [->* (json?)
                           (output-port?
                            symbol?
                            #:encode  encode?
                            #:format? boolean?
                            #:indent  string?)
                           void?]]
          [read-JSON  [->* (#:mutable? boolean?)
                           (input-port? symbol?)
                           (or/c eof-object? json?)]]

          ;; Conversion
          [json-copy    [->  json? #:mutable? boolean? json?]]
          [jsexpr->json [->* (any/c    ; not jsexpr?, because #:null may be used
                              #:mutable? boolean?)
                             (#:null any/c
                              #:inf+ any/c
                              #:inf- any/c)
                             json?]]

          [json->jsexpr [->* (json?)
                             (#:null any/c
                              #:inf+ any/c
                              #:inf- any/c
                              #:mhash? boolean?)
                             any/c]]

          [json->string [->* (json?)
                             (symbol?
                              #:encode  encode?
                              #:format? boolean?
                              #:indent  string?)
                             string?]]
          [json->bytes  [->* (json?)
                             (symbol?
                              #:encode  encode?
                              #:format? boolean?
                              #:indent  string?)
                             bytes?]]
          [string->json [->* (string? #:mutable? boolean?)
                             (symbol?)
                             (or/c eof-object? json?)]]
          [bytes->json  [->* (bytes?  #:mutable? boolean?)
                             (symbol?)
                             (or/c eof-object? json?)]]))


(: json? [-> Any Boolean])
(define (json? x) (or (immutable-json? x) (mutable-json? x)))

(: mutable-json? [-> Any Boolean])
(define (mutable-json? x)
  (or (json-constant? x)
      (null? x)
      (and (mlist? x) (andmmap mutable-json? x))
      (and (hash? x) (not (immutable? x))
           (for/and ([(k v) (in-hash x)])
             (and (symbol? k) (mutable-json? v))))))
