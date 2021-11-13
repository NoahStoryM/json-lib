#lang racket/base

;; -----------------------------------------------------------------------------
;; DEPENDENCIES

;; racket/contract must come before provide
(require "untyped-help.rkt")

;; -----------------------------------------------------------------------------
;; SERVICES

(provide (all-defined-out))

;; -----------------------------------------------------------------------------
;; TYPE and PREDICATE

(define mhash?
  (λ (arg)
    (and (hash? arg)
         (hash-strong? arg)
         (not (immutable? arg)))))

(define (inexact-real-inf? arg) (ormap (λ (inf) (eqv? arg inf)) '(+inf.0 +inf.f -inf.0 -inf.f)))
(define (inexact-real-nan? arg) (ormap (λ (inf) (eqv? arg inf)) '(+nan.0 +nan.f -nan.0 -nan.f)))

(define (inexact-rational? arg)
  (and (inexact-real? arg)
       (not (or (inexact-real-inf? arg)
                (inexact-real-nan? arg)))))

(define (json-number? arg)
  (or (exact-integer? arg)
      (inexact-rational? arg)
      (JSON-inf? arg)))

(struct JSON-inf+ ()
  #:transparent
  #:constructor-name make-JSON-inf+)
(struct JSON-inf- ()
  #:transparent
  #:constructor-name make-JSON-inf-)
(define (JSON-inf? arg) (or (JSON-inf-? arg) (JSON-inf+? arg)))

(struct JSON-null ()
  #:transparent
  #:constructor-name make-JSON-null)

(define (json-constant? arg)
  (or (json-number? arg)
      (JSON-null? arg)
      (boolean? arg)
      (string? arg)))

(define (json-list? arg)
  (and (list? arg)
       (andmap immutable-json? arg)))
(define (json-hash? arg)
  (and (hash? arg) (immutable? arg)
       (for/and ([(k v) (in-hash arg)])
         (and (symbol? k) (immutable-json? v)))))

(define (mutable-json? x)
  (or (json-constant? x)
      (null? x)
      (and (mlist? x) (andmmap mutable-json? x))
      (and (hash? x) (not (immutable? x))
           (for/and ([(k v) (in-hash x)])
             (and (symbol? k) (mutable-json? v))))))
(define (immutable-json? arg)
  (or (json-constant? arg)
      (json-list? arg)
      (json-hash? arg)))
(define (json? x) (or (immutable-json? x) (mutable-json? x)))

(define (encode? arg)
  (ormap (λ (enc) (eqv? arg enc))
         '(control all)))
