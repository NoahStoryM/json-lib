#lang racket/base

(require racket/unit
         "../types.rkt"
         "../untyped-help.rkt"
         "../Custom/custom-sig.rkt"
         "../IO/io-sig.rkt"
         "../JSON/json-sig.rkt"
         "jsexpr-sig.rkt")

(provide jsexpr@)


(define-unit jsexpr@
  (import custom^ io^ json^)
  (export jsexpr^)

  ;; -----------------------------------------------------------------------------
  ;; MORE PREDICATE

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
            (and (hash? x) (for/and ([(k v) (in-hash x)])
                             (and (symbol? k) (loop v))))))))

  ;; -----------------------------------------------------------------------------
  ;; GENERATION  (from Racket to JSON)

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

  (define (read-jsexpr [i (current-input-port)]
                       [who 'read-jsexpr]
                       #:null   [jsnull (json-null)]
                       #:inf+   [jsinf+ (json-inf+)]
                       #:inf-   [jsinf- (json-inf-)]
                       #:mhash? [jsmhash? (jsexpr-mhash?)])
    (parameterize ([json-null jsnull]
                   [json-inf+ jsinf+]
                   [json-inf- jsinf-]
                   [jsexpr-mhash? jsmhash?])
      (define js (read-JSON* who i #:mutable? #f))
      (if (eof-object? js)
          eof
          (json->jsexpr js))))

  ;; -----------------------------------------------------------------------------
  ;; CONVERSION

(define (jsexpr-copy x
                     #:null   [jsnull   (json-null)]
                     #:inf+   [jsinf+   (json-inf+)]
                     #:inf-   [jsinf-   (json-inf-)]
                     #:mhash? [jsmhash? (jsexpr-mhash?)])
  (parameterize ([json-null jsnull]
                 [json-inf+ jsinf+]
                 [json-inf- jsinf-]
                 [jsexpr-mhash? jsmhash?])
    (cond
      [(or (eq? x (json-inf+)) (equal? x json-inf+)) x]
      [(or (eq? x (json-inf-)) (equal? x json-inf-)) x]
      [(or (eq? x (json-null)) (equal? x json-null)) x]
      [(json-constant? x) x]
      [(list? x) (map jsexpr-copy x)]
      [(hash? x)
       (cond
         [(jsexpr-mhash?)
          (define result (make-hasheq))
          (for ([(k v) (in-hash x)])
            (hash-set! result k (jsexpr-copy v)))
          result]
         [else
          (for/hasheq ([(k v) (in-hash x)])
            (values k (jsexpr-copy v)))])]
      [else (raise-type-error 'jsexpr-copy "jsexpr?" x)])))

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
         (cond [(JSON-inf+? js) (json-inf+)]
               [(JSON-inf-? js) (json-inf-)]
               [(JSON-null? js) (json-null)]
               [else js])]
        [(immutable-json? js)
         (cond [(json-list? js) (map json->jsexpr js)]
               [(json-hash? js)
                (cond
                  [(jsexpr-mhash?)
                   (define result (make-hasheq))
                   (for ([(k v) (in-hash js)])
                     (hash-set! result k (json->jsexpr v)))
                   result]
                  [else
                   (for/hasheq
                       ([(k v) (in-hash js)])
                     (values k (json->jsexpr v)))])])]
        [else
         (cond [(null? js) '()]
               [(mpair? js) (mmap->list json->jsexpr js)]
               [(hash? js)
                (cond
                  [(jsexpr-mhash?)
                   (define result (make-hasheq))
                   (for ([(k v) (in-hash js)])
                     (hash-set! result k (json->jsexpr v)))
                   result]
                  [else
                   (for/hasheq
                       ([(k v) (in-hash js)])
                     (values k (json->jsexpr v)))])])])))


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
