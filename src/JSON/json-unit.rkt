#lang typed/racket/base

(require typed/racket/unit
         "../types.rkt"
         "../help.rkt"
         "../IO/io-sig.rkt"
         "json-sig.rkt")

(provide json@)


(define-unit json@
  (import io^)
  (export json^)

  ;; -----------------------------------------------------------------------------
  ;; MORE PREDICATE

  (: mjson? [-> JSON Boolean : Mutable-JSON])
  (define (mjson? js)
    (or (json-constant? js)
        (json-mlist? js)
        (json-mhash? js)))

  (: json-mlist? [-> JSON Boolean : JS-MList])
  (define (json-mlist? js) (or (null? js) (mpair? js)))

  ;;; TODO
  ;; (: mjson? [-> (U EOF JSON) Boolean : Mutable-JSON])
  ;; (define (mjson? js)
  ;;   (and (not (eof-object? js))
  ;;        (or (json-constant? js)
  ;;            (json-mhash? js)
  ;;            (json-mlist? js))))

  ;; (: json-mlist? [-> (U EOF JSON) Boolean : JS-MList])
  ;; (define (json-mlist? js)
  ;;   (and (not (eof-object? js))
  ;;        (or (null? js) (mpair? js))))

  (: json-mhash? [-> (U EOF JSON) Boolean : JS-MHash])
  (define (json-mhash? js) (and (not (eof-object? js)) (mhash? js)))

  ;; -----------------------------------------------------------------------------
  ;; GENERATION  (from Racket to JSON)

  (: write-JSON [->* (JSON) (Output-Port Symbol #:encode Encode) Void])
  (define (write-JSON js
                      [o (current-output-port)]
                      [who 'write-Immutable-JSON]
                      #:encode [enc 'control])
    (write-JSON* who js o enc))

  ;; -----------------------------------------------------------------------------
  ;; PARSING (from JSON to Racket)

  (: read-JSON (case-> [->* (#:mutable? False) (Input-Port Symbol) (U EOF Immutable-JSON)]
                       [->* (#:mutable? True)  (Input-Port Symbol) (U EOF Mutable-JSON)]))
  (define (read-JSON #:mutable? mutable? [i (current-input-port)] [who 'read-JSON])
    (read-JSON* who i #:mutable? mutable?))

  ;; -----------------------------------------------------------------------------
  ;; CONVERSION

  (: jsexpr->json (case-> [-> JSExpr
                              #:mutable? False
                              [#:null JSExpr]
                              [#:inf+ JSExpr]
                              [#:inf- JSExpr]
                              Immutable-JSON]
                          [-> JSExpr
                              #:mutable? True
                              [#:null JSExpr]
                              [#:inf+ JSExpr]
                              [#:inf- JSExpr]
                              Mutable-JSON]))
  (define (jsexpr->json x
                        #:mutable? mutable?
                        #:null     [jsnull (json-null)]
                        #:inf+     [jsinf+ (json-inf+)]
                        #:inf-     [jsinf- (json-inf-)])
    (parameterize ([json-null jsnull]
                   [json-inf+ jsinf+]
                   [json-inf- jsinf-])
      (cond
        [(or (eq? x (json-inf+)) (equal? x json-inf+)) JSON-inf+]
        [(or (eq? x (json-inf-)) (equal? x json-inf-)) JSON-inf-]
        [(or (eq? x (json-null)) (equal? x json-null)) JSON-null]
        [(json-constant? x) x]
        [(eq? #f mutable?)
         (cond [(list? x) (map (λ (arg) (jsexpr->json arg #:mutable? #f)) x)]
               ;; [(mpair? x) (mlist->list (mmap (λ (arg) (jsexpr->json arg #:mutable? #f)) x))] ; TODO
               [(hash? x)
                (for/hasheq : JS-Hash
                    ([(k v) (in-hash x)])
                  (values (assert k symbol?) (jsexpr->json v #:mutable? #f)))]
               [else (raise-type-error 'jsexpr->json "jsexpr?" x)])]
        [(eq? #t mutable?)
         (cond [(list? x) (list->mlist (map (λ (arg) (jsexpr->json arg #:mutable? #t)) x))]
               ;; [(mpair? x) (mmap (λ (arg) (jsexpr->json arg #:mutable? #t)) x)] ; TODO
               [(hash? x)
                (: result JS-MHash)
                (define result (make-hasheq))
                (for ([(k v) (in-hash x)])
                  (hash-set! result (assert k symbol?) (jsexpr->json v #:mutable? #t)))
                result]
               [else (raise-type-error 'jsexpr->json "jsexpr?" x)])])))


  (: json->string [->* (JSON) (Symbol #:encode Encode) String])
  (define (json->string js [who 'json->string] #:encode [enc 'control])
    (define o (open-output-string))
    (write-JSON js o who #:encode enc)
    (get-output-string o))

  (: json->bytes [->* (JSON) (Symbol #:encode Encode) Bytes])
  (define (json->bytes js [who 'json->bytes] #:encode [enc 'control])
    (define o (open-output-bytes))
    (write-JSON js o who #:encode enc)
    (get-output-bytes o))

  (: string->json (case-> [->* (String #:mutable? False) (Symbol) (U EOF Immutable-JSON)]
                          [->* (String #:mutable? True ) (Symbol) (U EOF Mutable-JSON)]))
  (define (string->json str #:mutable? mutable? [who 'string->json])
    (define i (open-input-string str))
    (read-JSON i who #:mutable? mutable?))

  (: bytes->json (case-> [->* (Bytes #:mutable? False) (Symbol) (U EOF Immutable-JSON)]
                         [->* (Bytes #:mutable? True ) (Symbol) (U EOF Mutable-JSON)]))
  (define (bytes->json bs #:mutable? mutable? [who 'bytes->json])
    (define i (open-input-bytes bs))
    (read-JSON i who #:mutable? mutable?)))
