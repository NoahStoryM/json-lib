#lang racket/base

(require racket/unit
         "../types.rkt"
         "custom-sig.rkt")

(provide custom@)


(define-unit custom@
  (import)
  (export custom^)


  ;; -----------------------------------------------------------------------------
  ;; CUSTOMIZATION

  ;; The default translation for a JSON `null' value
  (define json-null (make-parameter 'null))
  (define JSON-null (make-JSON-null))


  ;; The default translation for a Racket `+inf' value
  (define json-inf+ (make-parameter +inf.0))
  (define JSON-inf+ (make-JSON-inf+))


  ;; The default translation for a Racket `-inf' value
  (define json-inf- (make-parameter -inf.0))
  (define JSON-inf- (make-JSON-inf-))

  (define jsexpr-mhash? (make-parameter #f)))
