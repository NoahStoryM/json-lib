#lang typed/racket/base

;; -----------------------------------------------------------------------------
;; SERVICES

(provide (all-defined-out))

;; -----------------------------------------------------------------------------
;; TYPE and PREDICATE

(define-predicate mhash? Mutable-HashTableTop)

(define-type Inexact-Real-Inf (U +inf.0 +inf.f -inf.0 -inf.f))
(define-predicate inexact-real-inf? Inexact-Real-Inf)
(define-predicate inexact-real-nan? Inexact-Real-Nan)

(define-type Inexact-Rational (Refine [n : Inexact-Real] (! n (U Inexact-Real-Nan Inexact-Real-Inf))))
(define-predicate inexact-rational? Inexact-Rational)

(define-type JS-Number (U Integer Inexact-Rational JS-Inf))
(define-predicate json-number? JS-Number)

(struct js-inf+ () #:transparent #:type-name JS-Pos-Inf)
(struct js-inf- () #:transparent #:type-name JS-Neg-Inf)
(define-type JS-Inf (U JS-Pos-Inf JS-Neg-Inf))
(define-predicate js-inf? JS-Inf)

(struct js-null () #:transparent #:type-name JS-Null)

(define-type JS-Constant (U JS-Number JS-Null Boolean String))
(define-predicate json-constant? JS-Constant)

(define-type JS-MList (MListof Mutable-JSON))
(define-type JS-List  (Listof  Immutable-JSON))
(define-predicate json-list? JS-List)

(define-type JS-MHash (Mutable-HashTable   Symbol Mutable-JSON))
(define-type JS-Hash  (Immutable-HashTable Symbol Immutable-JSON))
(define-predicate json-hash? JS-Hash)

(define-type Mutable-JSON   (U JS-Constant JS-MList JS-MHash))
(define-type Immutable-JSON (U JS-Constant JS-List  JS-Hash))
(define-type JSON (U Mutable-JSON Immutable-JSON))
(define-predicate immutable-json? Immutable-JSON)

(define-type JSExpr Any)

(define-type Encode (U 'control 'all))
