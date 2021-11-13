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

(define-type JSON-Number (U Integer Inexact-Rational JSON-Inf))
(define-predicate json-number? JSON-Number)

(struct JSON-inf+ ()
  #:transparent
  #:constructor-name make-JSON-inf+
  #:type-name JSON-Pos-Inf)
(struct JSON-inf- ()
  #:transparent
  #:constructor-name make-JSON-inf-
  #:type-name JSON-Neg-Inf)
(define-type JSON-Inf (U JSON-Pos-Inf JSON-Neg-Inf))
(define-predicate JSON-inf? JSON-Inf)

(struct JSON-null ()
  #:transparent
  #:constructor-name make-JSON-null
  #:type-name JSON-Null)

(define-type JSON-Constant (U JSON-Number JSON-Null Boolean String))
(define-predicate json-constant? JSON-Constant)

(define-type JSON-MList (MListof Mutable-JSON))
(define-type JSON-List  (Listof  Immutable-JSON))
(define-predicate json-list? JSON-List)

(define-type JSON-MHash (Mutable-HashTable   Symbol Mutable-JSON))
(define-type JSON-Hash  (Immutable-HashTable Symbol Immutable-JSON))
(define-predicate json-hash? JSON-Hash)

(define-type Mutable-JSON   (U JSON-Constant JSON-MList JSON-MHash))
(define-type Immutable-JSON (U JSON-Constant JSON-List  JSON-Hash))
(define-type JSON (U Mutable-JSON Immutable-JSON))
(define-predicate immutable-json? Immutable-JSON)

(define-type JSExpr Any)

(define-type Encode (U 'control 'all))
(define-predicate encode? Encode)
