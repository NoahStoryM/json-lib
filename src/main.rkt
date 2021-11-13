#lang racket/base

;; Roughly based on the PLaneT package by Dave Herman,
;;   Originally released under MIT license.

;; edited:
;; -- Matthias, organization in preparation for pretty-print
;; -- Matthias, contracts

;; -----------------------------------------------------------------------------
;; DEPENDENCIES

(require racket/unit
         racket/contract
         "types.rkt"
         "Custom/custom-sig.rkt"
         "Custom/custom-unit.rkt"
         "IO/io-sig.rkt"
         "IO/io-unit.rkt"
         "Format/format-sig.rkt"
         "Format/format-unit.rkt"
         "JSON/json-sig.rkt"
         "JSON/json-unit.rkt"
         "JSExpr/jsexpr-sig.rkt"
         "JSExpr/jsexpr-unit.rkt")

;; tests in:
;; ~plt/pkgs/racket-test/tests/json/

;; docs in:
;; ~plt/pkgs/racket-doc/json/

;; -----------------------------------------------------------------------------
;; SERVICES

(provide
 JSON-null JSON-inf+ JSON-inf-

 ;; Parameter
 json-null json-inf+ json-inf-
 jsexpr-mhash?

 (contract-out
  ;; Predicate
  [jsexpr?         [->* (any/c)
                        (#:null any/c
                         #:inf+ any/c
                         #:inf- any/c)
                        boolean?]]

  [immutable-json? [-> any/c boolean?]]
  [mutable-json?   [-> any/c boolean?]]
  [json?           [-> any/c boolean?]]
  [json-list?      [-> any/c boolean?]]
  [json-hash?      [-> any/c boolean?]]
  [json-constant?  [-> any/c boolean?]]
  [json-number?    [-> any/c boolean?]]

  [JSON-inf?       [-> any/c boolean?]]
  [JSON-inf+?      [-> any/c boolean?]]
  [JSON-inf-?      [-> any/c boolean?]]
  [JSON-null?      [-> any/c boolean?]]

  [mjson?          [-> json? boolean?]]
  [json-mlist?     [-> json? boolean?]]
  [json-mhash?     [-> (or/c eof-object? json?) boolean?]]

  [encode?         [-> any/c boolean?]]

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

  [write-jsexpr [->* (any/c)
                     (output-port?
                      symbol?
                      #:null any/c
                      #:inf+ any/c
                      #:inf- any/c
                      #:encode  encode?
                      #:format? boolean?
                      #:indent  string?)
                     void?]]
  [read-jsexpr  [->* ()
                     (input-port?
                      symbol?
                      #:null any/c
                      #:inf+ any/c
                      #:inf- any/c
                      #:mhash? boolean?)
                     any]]

  ;; Conversion
  [json-copy    [->  json? #:mutable? boolean? json?]]
  [jsexpr->json [->* (any/c #:mutable? boolean?)
                     (#:null any/c
                      #:inf+ any/c
                      #:inf- any/c)
                     json?]]

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
                     (or/c eof-object? json?)]]


  [jsexpr-copy    [->* (any/c)
                       (#:null any/c
                        #:inf+ any/c
                        #:inf- any/c
                        #:mhash? boolean?)
                       any]]

  [json->jsexpr   [->* (json?)
                       (#:null any/c
                        #:inf+ any/c
                        #:inf- any/c
                        #:mhash? boolean?)
                       any]]

  [jsexpr->string [->* (any/c)
                       (symbol?
                        #:null any/c
                        #:inf+ any/c
                        #:inf- any/c
                        #:encode  encode?
                        #:format? boolean?
                        #:indent  string?)
                       string?]]
  [jsexpr->bytes  [->* (any/c)
                       (symbol?
                        #:null any/c
                        #:inf+ any/c
                        #:inf- any/c
                        #:encode  encode?
                        #:format? boolean?
                        #:indent  string?)
                       bytes?]]
  [string->jsexpr [->* (string?)
                       (symbol?
                        #:null any/c
                        #:inf+ any/c
                        #:inf- any/c
                        #:mhash? boolean?)
                       any]]
  [bytes->jsexpr  [->* (bytes?)
                       (symbol?
                        #:null any/c
                        #:inf+ any/c
                        #:inf- any/c
                        #:mhash? boolean?)
                       any]]

  ;; Format
  [format-json [->* ((or/c string? bytes?)
                     #:type (or/c 'string 'bytes))
                    (#:encode  encode?
                     #:indent  string?)
                    (or/c string? bytes?)]]))


(define-compound-unit/infer base@
  (import)
  (export custom^ io^ format^ json^ jsexpr^)
  (link   custom@ io@ format@ json@ jsexpr@))

(define-values/invoke-unit base@
  (import)
  (export custom^ io^ format^ json^ jsexpr^))
