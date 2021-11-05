#lang typed/racket/base

(require typed/racket/unit
         "../types.rkt"
         "../help.rkt"
         "../IO/io-sig.rkt"
         "convert-sig.rkt")

(provide convert@)


(define-unit convert@
  (import io^)
  (export convert^)

  (: json->jsexpr [-> JSON
                      [#:null JSExpr]
                      [#:inf+ JSExpr]
                      [#:inf- JSExpr]
                      [#:mhash? Boolean]
                      JSExpr])
  (define (json->jsexpr js
                        #:null   [jsnull   (json-null)]
                        #:inf+   [jsinf+   (json-inf+)]
                        #:inf-   [jsinf-   (json-inf-)]
                        #:mhash? [jsmhash? (jsexpr-mhash?)])
    (parameterize ([json-null jsnull]
                   [json-inf+ jsinf+]
                   [json-inf- jsinf-]
                   [jsexpr-mhash? jsmhash?])
      (cond
        [(json-constant? js)
         (cond [(js-inf+? js) (json-inf+)]
               [(js-inf-? js) (json-inf-)]
               [(js-null? js) (json-null)]
               [else js])]
        [(immutable-json? js)
         (cond [(json-list? js) (map json->jsexpr js)]
               [(json-hash? js)
                (cond
                  [(jsexpr-mhash?)
                   (: result (Mutable-HashTable Symbol JSExpr))
                   (define result (make-hasheq))
                   (for ([(k v) (in-hash js)])
                     (hash-set! result k v))
                   result]
                  [else
                   (for/hasheq : (Immutable-HashTable Symbol JSExpr)
                       ([(k v) (in-hash js)])
                     (values k (json->jsexpr v)))])])]
        [else
         (cond [(null? js) '()]
               [(mpair? js) (mlist->list (mmap json->jsexpr js))]
               [(hash? js)
                (cond
                  [(jsexpr-mhash?)
                   (: result (Mutable-HashTable Symbol JSExpr))
                   (define result (make-hasheq))
                   (for ([(k v) (in-hash js)])
                     (hash-set! result k v))
                   result]
                  [else
                   (for/hasheq : (Immutable-HashTable Symbol JSExpr)
                       ([(k v) (in-hash js)])
                     (values k (json->jsexpr v)))])])])))

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
    (read-JSON i who #:mutable? mutable?))


  (: jsexpr->string [->* (JSExpr)
                         (Symbol
                          #:null JSExpr
                          #:inf+ JSExpr
                          #:inf- JSExpr
                          #:encode Encode)
                         String])
  (define (jsexpr->string x
                          [who 'jsexpr->string]
                          #:null [jsnull (json-null)]
                          #:inf+ [jsinf+ (json-inf+)]
                          #:inf- [jsinf- (json-inf-)]
                          #:encode [enc 'control])
    (parameterize ([json-null jsnull]
                   [json-inf+ jsinf+]
                   [json-inf- jsinf-])
      (json->string (jsexpr->json x #:mutable? #f) who #:encode enc)))

  (: jsexpr->bytes [->* (JSExpr)
                        (Symbol
                         #:null JSExpr
                         #:inf+ JSExpr
                         #:inf- JSExpr
                         #:encode Encode)
                        Bytes])
  (define (jsexpr->bytes x
                         [who 'jsexpr->bytes]
                         #:null [jsnull (json-null)]
                         #:inf+ [jsinf+ (json-inf+)]
                         #:inf- [jsinf- (json-inf-)]
                         #:encode [enc 'control])
    (parameterize ([json-null jsnull]
                   [json-inf+ jsinf+]
                   [json-inf- jsinf-])
      (json->bytes (jsexpr->json x #:mutable? #f) who #:encode enc)))

  (: string->jsexpr [->* (String)
                         (Symbol
                          #:null JSExpr
                          #:inf+ JSExpr
                          #:inf- JSExpr
                          #:mhash? Boolean)
                         (U EOF JSExpr)])
  (define (string->jsexpr str
                          [who 'string->jsexpr]
                          #:null [jsnull (json-null)]
                          #:inf+ [jsinf+ (json-inf+)]
                          #:inf- [jsinf- (json-inf-)]
                          #:mhash? [jsmhash? (jsexpr-mhash?)])
    (define i (open-input-string str))
    (parameterize ([json-null jsnull]
                   [json-inf+ jsinf+]
                   [json-inf- jsinf-]
                   [jsexpr-mhash? jsmhash?])
      (define js (read-JSON i who #:mutable? #f))
      (if (eof-object? js)
          eof
          (json->jsexpr js))))

  (: bytes->jsexpr [->* (Bytes)
                        (Symbol
                         #:null JSExpr
                         #:inf+ JSExpr
                         #:inf- JSExpr
                         #:mhash? Boolean)
                        (U EOF JSExpr)])
  (define (bytes->jsexpr bs
                         [who 'bytes->jsexpr]
                         #:null [jsnull (json-null)]
                         #:inf+ [jsinf+ (json-inf+)]
                         #:inf- [jsinf- (json-inf-)]
                         #:mhash? [jsmhash? (jsexpr-mhash?)])
    (define i (open-input-bytes bs))
    (parameterize ([json-null jsnull]
                   [json-inf+ jsinf+]
                   [json-inf- jsinf-]
                   [jsexpr-mhash? jsmhash?])
      (define js (read-JSON i who #:mutable? #f))
      (if (eof-object? js)
          eof
          (json->jsexpr js)))))
