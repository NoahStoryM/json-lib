#lang typed/racket/base

(require typed/racket/unit
         "../types.rkt"
         "../typed-help.rkt"
         "../IO/io-sig.rkt"
         "../JSON/json-sig.rkt"
         "jsexpr-sig.rkt")

(provide jsexpr@)


(module untyped racket/base
  (provide jsexpr? jsexpr-copy)
  (require "../types.rkt"
           "../untyped-help.rkt")

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
            (and (mlist? x) (andmmap loop x))
            (and (hash? x) (for/and ([(k v) (in-hash x)])
                             (and (symbol? k) (loop v))))))))

  (define (jsexpr-copy x
                     #:null   [jsnull   (json-null)]
                     #:inf+   [jsinf+   (json-inf+)]
                     #:inf-   [jsinf-   (json-inf-)]
                     #:mlist? [jsmlist? (jsexpr-mlist?)]
                     #:mhash? [jsmhash? (jsexpr-mhash?)])
  (parameterize ([json-null jsnull]
                 [json-inf+ jsinf+]
                 [json-inf- jsinf-]
                 [jsexpr-mlist? jsmlist?]
                 [jsexpr-mhash? jsmhash?])
    (cond
      [(or (eq? x (json-inf+)) (equal? x json-inf+)) x]
      [(or (eq? x (json-inf-)) (equal? x json-inf-)) x]
      [(or (eq? x (json-null)) (equal? x json-null)) x]
      [(json-constant? x) x]
      [(list? x)
       (if (jsexpr-mlist?)
           (map->mlist jsexpr-copy x)
           (map jsexpr-copy x))]
      [(mpair? x)
       (if (jsexpr-mlist?)
           (mmap jsexpr-copy x)
           (mmap->list jsexpr-copy x))]
      [(hash? x)
       (cond
         [(jsexpr-mhash?)
          (define result (make-hasheq))
          (for ([(k v) (in-hash x)])
            (hash-set! result k (jsexpr-copy v)))
          result]
         [else
          (for/hasheq ([(k v) (in-hash x)])
            (values k (jsexpr-copy v)))])]))))
(require/typed 'untyped
  [[jsexpr? untyped/jsexpr?]
   [-> Any
       [#:null JSExpr]
       [#:inf+ JSExpr]
       [#:inf- JSExpr]
       Boolean]]
  [[jsexpr-copy untyped/jsexpr-copy]
   [-> JSExpr
       [#:null JSExpr]
       [#:inf+ JSExpr]
       [#:inf- JSExpr]
       [#:mlist? Boolean]
       [#:mhash? Boolean]
       JSExpr]])

(define-unit jsexpr@
  (import io^ json^)
  (export jsexpr^)

  ;; -----------------------------------------------------------------------------
  ;; MORE PREDICATE

  (: jsexpr? [-> Any
                 [#:null JSExpr]
                 [#:inf+ JSExpr]
                 [#:inf- JSExpr]
                 Boolean])
  (define jsexpr? untyped/jsexpr?)

  ;; -----------------------------------------------------------------------------
  ;; GENERATION  (from Racket to JSON)

  (: write-jsexpr [->* (JSExpr)
                       (Output-Port
                        Symbol
                        #:null JSExpr
                        #:inf+ JSExpr
                        #:inf- JSExpr
                        #:encode  Encode
                        #:format? Boolean
                        #:indent  String)
                       Void])
  (define (write-jsexpr x
                        [o (current-output-port)]
                        [who 'write-jsexpr]
                        #:null [jsnull (json-null)]
                        #:inf+ [jsinf+ (json-inf+)]
                        #:inf- [jsinf- (json-inf-)]
                        #:encode  [enc 'control]
                        #:format? [format? #f]
                        #:indent  [indent  "\t"])
    (parameterize ([json-null jsnull]
                   [json-inf+ jsinf+]
                   [json-inf- jsinf-])
      (write-JSON (jsexpr->json x #:mutable? #f) o who
                  #:encode  enc
                  #:format? format?
                  #:indent  indent)))

  ;; -----------------------------------------------------------------------------
  ;; PARSING (from JSON to Racket)

  (: read-jsexpr [->* ()
                      (Input-Port
                       Symbol
                       #:null JSExpr
                       #:inf+ JSExpr
                       #:inf- JSExpr
                       #:mlist? Boolean
                       #:mhash? Boolean)
                      JSExpr])
  (define (read-jsexpr [i (current-input-port)]
                       [who 'read-jsexpr]
                       #:null   [jsnull (json-null)]
                       #:inf+   [jsinf+ (json-inf+)]
                       #:inf-   [jsinf- (json-inf-)]
                       #:mlist? [jsmlist? (jsexpr-mlist?)]
                       #:mhash? [jsmhash? (jsexpr-mhash?)])
    (parameterize ([json-null jsnull]
                   [json-inf+ jsinf+]
                   [json-inf- jsinf-]
                   [jsexpr-mlist? jsmlist?]
                   [jsexpr-mhash? jsmhash?])
      (define js (read-JSON* who i #:mutable? #f))
      (if (eof-object? js)
          eof
          (json->jsexpr js))))

  ;; -----------------------------------------------------------------------------
  ;; CONVERSION

  (: jsexpr-copy [-> JSExpr
                     [#:null JSExpr]
                     [#:inf+ JSExpr]
                     [#:inf- JSExpr]
                     [#:mlist? Boolean]
                     [#:mhash? Boolean]
                     JSExpr])
  (define jsexpr-copy untyped/jsexpr-copy)

  (: json->jsexpr [-> JSON
                      [#:null JSExpr]
                      [#:inf+ JSExpr]
                      [#:inf- JSExpr]
                      [#:mlist? Boolean]
                      [#:mhash? Boolean]
                      JSExpr])
  (define (json->jsexpr js
                        #:null   [jsnull   (json-null)]
                        #:inf+   [jsinf+   (json-inf+)]
                        #:inf-   [jsinf-   (json-inf-)]
                        #:mlist? [jsmlist? (jsexpr-mlist?)]
                        #:mhash? [jsmhash? (jsexpr-mhash?)])
    (parameterize ([json-null jsnull]
                   [json-inf+ jsinf+]
                   [json-inf- jsinf-]
                   [jsexpr-mlist? jsmlist?]
                   [jsexpr-mhash? jsmhash?])
      (cond
        [(json-constant? js)
         (cond [(js-inf+? js) (json-inf+)]
               [(js-inf-? js) (json-inf-)]
               [(js-null? js) (json-null)]
               [else js])]
        [(immutable-json? js)
         (cond [(json-list? js)
                (if (jsexpr-mlist?)
                    (map->mlist json->jsexpr js)
                    (map json->jsexpr js))]
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
               [(mpair? js)
                (if (jsexpr-mlist?)
                    (mmap json->jsexpr js)
                    (mmap->list json->jsexpr js))]
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


  (: jsexpr->string [->* (JSExpr)
                         (Symbol
                          #:null JSExpr
                          #:inf+ JSExpr
                          #:inf- JSExpr
                          #:encode  Encode
                          #:format? Boolean
                          #:indent  String)
                         String])
  (define (jsexpr->string x
                          [who 'jsexpr->string]
                          #:null [jsnull (json-null)]
                          #:inf+ [jsinf+ (json-inf+)]
                          #:inf- [jsinf- (json-inf-)]
                          #:encode  [enc 'control]
                          #:format? [format? #f]
                          #:indent  [indent  "\t"])
    (parameterize ([json-null jsnull]
                   [json-inf+ jsinf+]
                   [json-inf- jsinf-])
      (json->string (jsexpr->json x #:mutable? #f)
                    who
                    #:encode  enc
                    #:format? format?
                    #:indent  indent)))

  (: jsexpr->bytes [->* (JSExpr)
                        (Symbol
                         #:null JSExpr
                         #:inf+ JSExpr
                         #:inf- JSExpr
                         #:encode  Encode
                         #:format? Boolean
                         #:indent  String)
                        Bytes])
  (define (jsexpr->bytes x
                         [who 'jsexpr->bytes]
                         #:null [jsnull (json-null)]
                         #:inf+ [jsinf+ (json-inf+)]
                         #:inf- [jsinf- (json-inf-)]
                         #:encode  [enc 'control]
                         #:format? [format? #f]
                         #:indent  [indent  "\t"])
    (parameterize ([json-null jsnull]
                   [json-inf+ jsinf+]
                   [json-inf- jsinf-])
      (json->bytes (jsexpr->json x #:mutable? #f)
                   who
                   #:encode  enc
                   #:format? format?
                   #:indent  indent)))

  (: string->jsexpr [->* (String)
                         (Symbol
                          #:null JSExpr
                          #:inf+ JSExpr
                          #:inf- JSExpr
                          #:mlist? Boolean
                          #:mhash? Boolean)
                         (U EOF JSExpr)])
  (define (string->jsexpr str
                          [who 'string->jsexpr]
                          #:null [jsnull (json-null)]
                          #:inf+ [jsinf+ (json-inf+)]
                          #:inf- [jsinf- (json-inf-)]
                          #:mlist? [jsmlist? (jsexpr-mlist?)]
                          #:mhash? [jsmhash? (jsexpr-mhash?)])
    (define i (open-input-string str))
    (parameterize ([json-null jsnull]
                   [json-inf+ jsinf+]
                   [json-inf- jsinf-]
                   [jsexpr-mlist? jsmlist?]
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
                         #:mlist? Boolean
                         #:mhash? Boolean)
                        (U EOF JSExpr)])
  (define (bytes->jsexpr bs
                         [who 'bytes->jsexpr]
                         #:null [jsnull (json-null)]
                         #:inf+ [jsinf+ (json-inf+)]
                         #:inf- [jsinf- (json-inf-)]
                         #:mlist? [jsmlist? (jsexpr-mlist?)]
                         #:mhash? [jsmhash? (jsexpr-mhash?)])
    (define i (open-input-bytes bs))
    (parameterize ([json-null jsnull]
                   [json-inf+ jsinf+]
                   [json-inf- jsinf-]
                   [jsexpr-mlist? jsmlist?]
                   [jsexpr-mhash? jsmhash?])
      (define js (read-JSON i who #:mutable? #f))
      (if (eof-object? js)
          eof
          (json->jsexpr js)))))
