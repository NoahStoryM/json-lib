#lang racket/base

(require racket/unit
         "../types.rkt"
         "../untyped-help.rkt"
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

  (define (mjson? js)
    (or (json-constant? js)
        (json-mlist? js)
        (json-mhash? js)))

  (define (json-mlist? js) (or (null? js) (mpair? js)))

  (define (json-mhash? js) (and (not (eof-object? js)) (mhash? js)))

  ;; -----------------------------------------------------------------------------
  ;; GENERATION  (from Racket to JSON)

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

  (define (read-JSON #:mutable? mutable? [i (current-input-port)] [who 'read-JSON])
    (read-JSON* who i #:mutable? mutable?))

  ;; -----------------------------------------------------------------------------
  ;; CONVERSION

  (define json-copy
    (let ()
      (define (copy-immutable-json js)
        (cond
          [(immutable-json? js) js]
          [(json-mlist? js) (mlist->list (mmap copy-immutable-json js))]
          [(json-mhash? js)
           (for/hasheq
               ([(k v) (in-hash js)])
             (values k (copy-immutable-json v)))]))

      (define (copy-mutable-json js)
        (cond
          [(json-constant? js) js]
          [(json-list?  js) (mmap copy-mutable-json (list->mlist js))]
          [(json-mlist? js) (mmap copy-mutable-json js)]
          [(json-hash? js)
           (define result (make-hasheq))
           (for ([(k v) (in-hash js)])
             (hash-set! result k (copy-mutable-json v)))
           result]
          [(json-mhash? js)
           (define result (make-hasheq))
           (for ([(k v) (in-hash js)])
             (hash-set! result k (copy-mutable-json v)))
           result]
          [else (raise-type-error 'copy-mutable-json "json?" js)]))

      (λ (js #:mutable? mutable?)
        (cond [(eq? #f mutable?) (copy-immutable-json js)]
              [(eq? #t mutable?) (copy-mutable-json   js)]))))

  (define jsexpr->json
    (let ()
      (define (jsexpr->immutable-json x)
        (cond
          [(or (eqv? x (json-inf+)) (equal? x json-inf+)) JSON-inf+]
          [(or (eqv? x (json-inf-)) (equal? x json-inf-)) JSON-inf-]
          [(or (eqv? x (json-null)) (equal? x json-null)) JSON-null]
          [(json-constant? x) x]
          [(list? x)  (map jsexpr->immutable-json x)]
          [(hash? x)
           (for/hasheq
               ([(k v) (in-hash x)])
             (values k (jsexpr->immutable-json v)))]
          [else (raise-type-error 'jsexpr->immutable-json "jsexpr?" x)]))

      (define (jsexpr->mutable-json x)
        (cond
          [(or (eqv? x (json-inf+)) (equal? x json-inf+)) JSON-inf+]
          [(or (eqv? x (json-inf-)) (equal? x json-inf-)) JSON-inf-]
          [(or (eqv? x (json-null)) (equal? x json-null)) JSON-null]
          [(json-constant? x) x]
          [(list? x)  (map->mlist jsexpr->mutable-json x)]
          [(hash? x)
           (define result (make-hasheq))
           (for ([(k v) (in-hash x)])
             (hash-set! result k (jsexpr->mutable-json v)))
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

  (define (string->json str #:mutable? mutable? [who 'string->json])
    (define i (open-input-string str))
    (read-JSON i who #:mutable? mutable?))

  (define (bytes->json bs #:mutable? mutable? [who 'bytes->json])
    (define i (open-input-bytes bs))
    (read-JSON i who #:mutable? mutable?)))
