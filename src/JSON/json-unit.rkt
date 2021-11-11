#lang typed/racket/base

(require typed/racket/unit
         "../types.rkt"
         "../typed-help.rkt"
         "../Custom/custom-sig.rkt"
         "../IO/io-sig.rkt"
         "../JSExpr/jsexpr-sig.rkt"
         "json-sig.rkt")

(provide json@)


(define-unit json@
  (import custom^ io^ jsexpr^)
  (export json^)

  ;; -----------------------------------------------------------------------------
  ;; MORE PREDICATE

  (: mjson? [-> JSON Boolean : Mutable-JSON])
  (define (mjson? js)
    (or (json-constant? js)
        (json-mlist? js)
        (json-mhash? js)))

  (: json-mlist? [-> JSON Boolean : JSON-MList])
  (define (json-mlist? js) (or (null? js) (mpair? js)))

  ;;; TODO
  ;; (: mjson? [-> (U EOF JSON) Boolean : Mutable-JSON])
  ;; (define (mjson? js)
  ;;   (and (not (eof-object? js))
  ;;        (or (json-constant? js)
  ;;            (json-mhash? js)
  ;;            (json-mlist? js))))

  ;; (: json-mlist? [-> (U EOF JSON) Boolean : JSON-MList])
  ;; (define (json-mlist? js)
  ;;   (and (not (eof-object? js))
  ;;        (or (null? js) (mpair? js))))

  (: json-mhash? [-> (U EOF JSON) Boolean : JSON-MHash])
  (define (json-mhash? js) (and (not (eof-object? js)) (mhash? js)))

  ;; -----------------------------------------------------------------------------
  ;; GENERATION  (from Racket to JSON)

  (: write-JSON [->* (JSON)
                     (Output-Port
                      Symbol
                      #:encode  Encode
                      #:format? Boolean
                      #:indent  String)
                     Void])
  (define (write-JSON js
                      [o (current-output-port)]
                      [who 'write-Immutable-JSON]
                      #:encode  [enc 'control]
                      #:format? [format? #f]
                      #:indent  [indent  "\t"])
    (write-JSON* who js o
                 #:encode  enc
                 #:format? format?
                 #:indent  indent))

  ;; -----------------------------------------------------------------------------
  ;; PARSING (from JSON to Racket)

  (: read-JSON (case-> [->* (#:mutable? False) (Input-Port Symbol) (U EOF Immutable-JSON)]
                       [->* (#:mutable? True ) (Input-Port Symbol) (U EOF Mutable-JSON)]))
  (define (read-JSON #:mutable? mutable? [i (current-input-port)] [who 'read-JSON])
    (read-JSON* who i #:mutable? mutable?))

  ;; -----------------------------------------------------------------------------
  ;; CONVERSION

  (: json-copy (case-> [-> JSON #:mutable? False Immutable-JSON]
                       [-> JSON #:mutable? True  Mutable-JSON]))
  (define json-copy
    (let ()
      (: copy-immutable-json [-> JSON Immutable-JSON])
      (define (copy-immutable-json js)
        (cond
          [(immutable-json? js) js]
          [(json-mlist? js) (mlist->list (mmap copy-immutable-json js))]
          [(json-mhash? js)
           (for/hasheq : JSON-Hash
               ([(k v) (in-hash js)])
             (values k (copy-immutable-json v)))]))

      (: copy-mutable-json [-> JSON Mutable-JSON])
      (define (copy-mutable-json js)
        (cond
          [(json-constant? js) js]
          [(json-list?  js) (mmap copy-mutable-json (list->mlist js))]
          [(json-mlist? js) (mmap copy-mutable-json js)]
          [(json-hash? js)
           (: result JSON-MHash)
           (define result (make-hasheq))
           (for ([(k v) (in-hash js)])
             (hash-set! result k (copy-mutable-json v)))
           result]
          [(json-mhash? js)
           (: result JSON-MHash)
           (define result (make-hasheq))
           (for ([(k v) (in-hash js)])
             (hash-set! result k (copy-mutable-json v)))
           result]
          [else (raise-type-error 'copy-mutable-json "json?" js)]))

      (λ (js #:mutable? mutable?)
        (cond [(eq? #f mutable?) (copy-immutable-json js)]
              [(eq? #t mutable?) (copy-mutable-json   js)]))))

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
  (define jsexpr->json
    (let ()
      (: jsexpr->immutable-json [-> JSExpr Immutable-JSON])
      (define (jsexpr->immutable-json x)
        (cond
          [(or (eq? x (json-inf+)) (equal? x json-inf+)) JSON-inf+]
          [(or (eq? x (json-inf-)) (equal? x json-inf-)) JSON-inf-]
          [(or (eq? x (json-null)) (equal? x json-null)) JSON-null]
          [(json-constant? x) x]
          [(list? x)  (map jsexpr->immutable-json x)]
          [(hash? x)
           (for/hasheq : JSON-Hash
               ([(k v) (in-hash x)])
             (values (assert k symbol?) (jsexpr->immutable-json v)))]
          [else (raise-type-error 'jsexpr->immutable-json "jsexpr?" x)]))

      (: jsexpr->mutable-json   [-> JSExpr Mutable-JSON])
      (define (jsexpr->mutable-json x)
        (cond
          [(or (eq? x (json-inf+)) (equal? x json-inf+)) JSON-inf+]
          [(or (eq? x (json-inf-)) (equal? x json-inf-)) JSON-inf-]
          [(or (eq? x (json-null)) (equal? x json-null)) JSON-null]
          [(json-constant? x) x]
          [(list? x)  (map->mlist jsexpr->mutable-json x)]
          [(hash? x)
           (: result JSON-MHash)
           (define result (make-hasheq))
           (for ([(k v) (in-hash x)])
             (hash-set! result (assert k symbol?) (jsexpr->mutable-json v)))
           result]
          [else (raise-type-error 'jsexpr->mutable-json "jsexpr?" x)]))

      (λ (x #:mutable? mutable?
            #:null     [jsnull (json-null)]
            #:inf+     [jsinf+ (json-inf+)]
            #:inf-     [jsinf- (json-inf-)])
        (parameterize ([json-null jsnull]
                       [json-inf+ jsinf+]
                       [json-inf- jsinf-])
          (cond [(eq? #f mutable?) (jsexpr->immutable-json x)]
                [(eq? #t mutable?) (jsexpr->mutable-json   x)])))))


  (: json->string [->* (JSON)
                       (Symbol
                        #:encode  Encode
                        #:format? Boolean
                        #:indent  String)
                       String])
  (define (json->string js
                        [who 'json->string]
                        #:encode  [enc 'control]
                        #:format? [format? #f]
                        #:indent  [indent  "\t"])
    (define o (open-output-string))
    (write-JSON js o who
                #:encode  enc
                #:format? format?
                #:indent  indent)
    (get-output-string o))

  (: json->bytes [->* (JSON)
                      (Symbol
                       #:encode  Encode
                       #:format? Boolean
                       #:indent  String)
                      Bytes])
  (define (json->bytes js
                       [who 'json->bytes]
                       #:encode  [enc 'control]
                       #:format? [format? #f]
                       #:indent  [indent  "\t"])
    (define o (open-output-bytes))
    (write-JSON js o who
                #:encode  enc
                #:format? format?
                #:indent  indent)
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
