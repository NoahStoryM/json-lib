#lang typed/racket/base

(require typed/racket/unit
         "../types.rkt"
         "custom-sig.rkt")

(provide custom@)


(define-unit custom@
  (import)
  (export custom^)


  ;; -----------------------------------------------------------------------------
  ;; CUSTOMIZATION

  ;; The default translation for a JSON `null' value
  (: json-null (Parameter JSExpr))
  (define json-null (make-parameter 'null))

  (: JSON-null JSON-Null)
  (define JSON-null (js-null))


  ;; The default translation for a Racket `+inf' value
  (: json-inf+ (Parameter JSExpr))
  (define json-inf+ (make-parameter +inf.0))

  (: JSON-inf+ JSON-Pos-Inf)
  (define JSON-inf+ (js-inf+))


  ;; The default translation for a Racket `-inf' value
  (: json-inf- (Parameter JSExpr))
  (define json-inf- (make-parameter -inf.0))

  (: JSON-inf- JSON-Neg-Inf)
  (define JSON-inf- (js-inf-))


  (: jsexpr-mhash? (Parameter Boolean))
  (define jsexpr-mhash? (make-parameter #f)))
