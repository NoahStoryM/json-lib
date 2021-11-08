#lang typed/racket/base/no-check

(require "../types.rkt"
         "../untyped-help.rkt")

(provide jsexpr? jsexpr-copy)

;; -----------------------------------------------------------------------------
;; MORE PREDICATE

(: jsexpr? [-> Any
               [#:null JSExpr]
               [#:inf+ JSExpr]
               [#:inf- JSExpr]
               Boolean])
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

;; -----------------------------------------------------------------------------
;; CONVERSION

(: jsexpr-copy [-> JSExpr
                   [#:null JSExpr]
                   [#:inf+ JSExpr]
                   [#:inf- JSExpr]
                   [#:mlist? Boolean]
                   [#:mhash? Boolean]
                   JSExpr])
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
          (: result (Mutable-HashTable Symbol JSExpr))
          (define result (make-hasheq))
          (for ([(k v) (in-hash x)])
            (hash-set! result (assert k symbol?) (jsexpr-copy v)))
          result]
         [else
          (for/hasheq : (Immutable-HashTable Symbol JSExpr)
              ([(k v) (in-hash x)])
            (values (assert k symbol?) (jsexpr-copy v)))])])))
