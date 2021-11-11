#lang typed/racket/base

;; Roughly based on the PLaneT package by Dave Herman,
;;   Originally released under MIT license.

;; edited:
;; -- Matthias, organization in preparation for pretty-print
;; -- Matthias, contracts

;; -----------------------------------------------------------------------------
;; DEPENDENCIES

(require typed/racket/unit
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
 ;; Parameter
 json-null json-inf+ json-inf-
 jsexpr-mhash?

 ;; Type and Predicate
 JSON
 Mutable-JSON   mjson?
 Immutable-JSON immutable-json?
 JSExpr         jsexpr?

 JSON-List json-list? JSON-MList json-mlist?
 JSON-Hash json-hash? JSON-MHash json-mhash?

 JSON-Constant json-constant?

 JSON-Number   json-number?
 JSON-Inf      js-inf?
 JSON-Pos-Inf  js-inf+? JSON-inf+
 JSON-Neg-Inf  js-inf-? JSON-inf-

 JSON-Null     js-null? JSON-null

 Encode

 ;; IO
 write-JSON read-JSON

 write-jsexpr (rename-out [write-jsexpr write-json])
 read-jsexpr  (rename-out [read-jsexpr  read-json])

 ;; Format
 format-json

 ;; Conversion Functions
 json-copy    jsexpr-copy
 json->jsexpr jsexpr->json

 json->string string->json
 json->bytes  bytes->json

 jsexpr->string string->jsexpr
 jsexpr->bytes  bytes->jsexpr
 )


(define-compound-unit/infer base@
  (import)
  (export custom^ io^ format^ json^ jsexpr^)
  (link   custom@ io@ format@ json@ jsexpr@))

(define-values/invoke-unit base@
  (import)
  (export custom^ io^ format^ json^ jsexpr^))
