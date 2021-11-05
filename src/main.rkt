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
         "IO/io-sig.rkt"
         "IO/io-unit.rkt"
         "Conversion/convert-sig.rkt"
         "Conversion/convert-unit.rkt")

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
 Mutable-JSON     mutable-json?
 Immutable-JSON   immutable-json?
 JSON             json?
 JSExpr           jsexpr?

 JS-MList JS-List json-list?
 JS-MHash JS-Hash json-hash?

 JS-Constant json-constant?

 JS-Number   json-number?
 JS-Inf      js-inf?
 JS-Pos-Inf  js-inf+? JSON-inf+
 JS-Neg-Inf  js-inf-? JSON-inf-

 JS-Null     js-null? JSON-null

 ;; IO
 write-JSON read-JSON

 write-jsexpr (rename-out [write-jsexpr write-json])
 read-jsexpr  (rename-out [read-jsexpr  read-json])

 ;; Conversion Functions
 json->jsexpr jsexpr->json

 json->string string->json
 json->bytes  bytes->json

 jsexpr->string string->jsexpr
 jsexpr->bytes  bytes->jsexpr
 )


(define-compound-unit/infer base@
  (import)
  (export io^ convert^)
  (link   io@ convert@))

(define-values/invoke-unit base@
  (import)
  (export io^ convert^))


(module more racket/base
  (provide mutable-json?)
  (require (only-in compatibility/mlist mlist?)
           "types.rkt")

  (define (mutable-json? x)
    (or (json-constant? x)
        (null? x)
        (and (mlist? x) (andmmap mutable-json? x))
        (and (hash? x) (not (immutable? x))
             (for/and ([(k v) (in-hash x)])
               (and (symbol? k) (mutable-json? v))))))

  ;; -----------------------------------------------------------------------------
  ;; CONVENIENCE

  (define (andmmap f l)
    (let loop ([l l])
      (if (null? l) #t (and (f (mcar l)) (loop (mcdr l)))))))
(require/typed 'more [mutable-json? [-> Any Boolean]])

(: json? [-> Any Boolean])
(define (json? x) (or (immutable-json? x) (mutable-json? x)))

(: jsexpr? [-> Any
               [#:null JSExpr]
               [#:inf+ JSExpr]
               [#:inf- JSExpr]
               Boolean])
(define (jsexpr? x
                 #:null [jsnull (json-null)]
                 #:inf+ [jsinf+ (json-inf+)]
                 #:inf- [jsinf- (json-inf-)])
  (parameterize ([json-null jsnull]
                 [json-inf+ jsinf+]
                 [json-inf- jsinf-])
    (let loop ([x x])
      (or (equal? x json-inf+) (eq? x (json-inf+))
          (equal? x json-inf-) (eq? x (json-inf-))
          (equal? x json-null) (eq? x (json-null))
          (json-constant? x)
          (and (list?  x) (andmap  loop x))
          ;; (and (mlist? x) (andmmap loop x)) ; TODO
          (and (hash? x) (for/and ([(k v) (in-hash x)])
                           (and (symbol? k) (loop v))))))))
