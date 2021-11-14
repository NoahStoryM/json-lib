#lang typed/racket/base

(require typed/racket/unsafe)

(module types typed/racket/base
  (provide
   JSON-Pos-Inf JSON-inf+?
   JSON-Neg-Inf JSON-inf-?
   JSON-Null    JSON-null?)

  (require/typed "src/types.rkt"
    ;; TYPE and PREDICATE
    [#:struct JSON-inf+ ()
     ;; #:transparent
     #:constructor-name make-JSON-inf+
     #:type-name JSON-Pos-Inf]
    [#:struct JSON-inf- ()
     ;; #:transparent
     #:constructor-name make-JSON-inf-
     #:type-name JSON-Neg-Inf]
    [#:struct JSON-null ()
     ;; #:transparent
     #:constructor-name make-JSON-null
     #:type-name JSON-Null]))
(require 'types)

(require/typed "untyped.rkt"
  [JSON-null JSON-Null]
  [JSON-inf+ JSON-Pos-Inf]
  [JSON-inf- JSON-Neg-Inf]

  ;; Parameter
  [json-null     (Parameter JSExpr)]
  [json-inf+     (Parameter JSExpr)]
  [json-inf-     (Parameter JSExpr)]
  [jsexpr-mhash? (Parameter Boolean)]

  ;; Predicate
  [jsexpr? [-> Any
               [#:null JSExpr]
               [#:inf+ JSExpr]
               [#:inf- JSExpr]
               Boolean]]

  ;; IO
  [write-jsexpr [->* (JSExpr)
                     (Output-Port
                      Symbol
                      #:null JSExpr
                      #:inf+ JSExpr
                      #:inf- JSExpr
                      #:encode  Encode
                      #:format? Boolean
                      #:indent  String)
                     Void]]
  [read-jsexpr  [->* ()
                     (Input-Port
                      Symbol
                      #:null JSExpr
                      #:inf+ JSExpr
                      #:inf- JSExpr
                      #:mhash? Boolean)
                     JSExpr]]

  ;; Conversion
  [jsexpr-copy    [-> JSExpr
                      [#:null JSExpr]
                      [#:inf+ JSExpr]
                      [#:inf- JSExpr]
                      [#:mhash? Boolean]
                      JSExpr]]
  [jsexpr->string [->* (JSExpr)
                       (Symbol
                        #:null JSExpr
                        #:inf+ JSExpr
                        #:inf- JSExpr
                        #:encode  Encode
                        #:format? Boolean
                        #:indent  String)
                       String]]
  [jsexpr->bytes  [->* (JSExpr)
                       (Symbol
                        #:null JSExpr
                        #:inf+ JSExpr
                        #:inf- JSExpr
                        #:encode  Encode
                        #:format? Boolean
                        #:indent  String)
                       Bytes]]
  [string->jsexpr [->* (String)
                       (Symbol
                        #:null JSExpr
                        #:inf+ JSExpr
                        #:inf- JSExpr
                        #:mhash? Boolean)
                       (U EOF JSExpr)]]
  [bytes->jsexpr  [->* (Bytes)
                       (Symbol
                        #:null JSExpr
                        #:inf+ JSExpr
                        #:inf- JSExpr
                        #:mhash? Boolean)
                       (U EOF JSExpr)]]
  )

(unsafe-require/typed "untyped.rkt"
  ;; Predicate
  [mutable-json? [-> Any Boolean : Mutable-JSON]]

  ;; IO
  [write-JSON [->* (JSON)
                   (Output-Port
                    Symbol
                    #:encode  Encode
                    #:format? Boolean
                    #:indent  String)
                   Void]]
  [read-JSON  (case-> [->* (#:mutable? False)
                           (Input-Port Symbol)
                           (U EOF Immutable-JSON)]
                      [->* (#:mutable? True)
                           (Input-Port Symbol)
                           (U EOF Mutable-JSON)])]

  ;; Conversion
  [json-copy    (case-> [-> JSON #:mutable? False Immutable-JSON]
                        [-> JSON #:mutable? True  Mutable-JSON])]
  [jsexpr->json (case-> [-> JSExpr
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
                            Mutable-JSON])]
  [json->jsexpr [-> JSON
                    [#:null JSExpr]
                    [#:inf+ JSExpr]
                    [#:inf- JSExpr]
                    [#:mhash? Boolean]
                    JSExpr]]

  [json->string [->* (JSON)
                     (Symbol
                      #:encode  Encode
                      #:format? Boolean
                      #:indent  String)
                     String]]
  [json->bytes  [->* (JSON)
                     (Symbol
                      #:encode  Encode
                      #:format? Boolean
                      #:indent  String)
                     Bytes]]

  [string->json (case-> [->* (String #:mutable? False)
                             (Symbol)
                             (U EOF Immutable-JSON)]
                        [->* (String #:mutable? True)
                             (Symbol)
                             (U EOF Mutable-JSON)])]
  [bytes->json  (case-> [->* (Bytes #:mutable? False)
                             (Symbol)
                             (U EOF Immutable-JSON)]
                        [->* (Bytes #:mutable? True)
                             (Symbol)
                             (U EOF Mutable-JSON)])]

  ;; Format
  [format-json (case-> [-> (U String Bytes)
                           #:type 'string
                           [#:encode Encode]
                           [#:indent String]
                           String]
                       [-> (U String Bytes)
                           #:type 'bytes
                           [#:encode Encode]
                           [#:indent String]
                           Bytes])]
  )

(provide
 ;; Parameter
 json-null json-inf+ json-inf-
 jsexpr-mhash?

 ;; Type and Predicate
 JSON
 Mutable-JSON   mjson?
 Immutable-JSON immutable-json?
 JSExpr         jsexpr?

 JSON-List json-list? JSON-MList json-mlist?
 JSON-Hash json-hash? JSON-MHash json-mhash?

 JSON-Constant json-constant?

 JSON-Number   json-number?
 JSON-Inf      JSON-inf?
 JSON-Pos-Inf  JSON-inf+? JSON-inf+
 JSON-Neg-Inf  JSON-inf-? JSON-inf-
 JSON-Null     JSON-null? JSON-null

 Encode        encode?

 ;; IO
 write-JSON read-JSON

 write-jsexpr (rename-out [write-jsexpr write-json])
 read-jsexpr  (rename-out [read-jsexpr  read-json])

 ;; Format
 format-json

 ;; Conversion Functions
 json-copy    jsexpr-copy
 json->jsexpr jsexpr->json

 json->string string->json
 json->bytes  bytes->json

 jsexpr->string string->jsexpr
 jsexpr->bytes  bytes->jsexpr
 )


;; -----------------------------------------------------------------------------
;; TYPE and PREDICATE

(define-predicate mhash? Mutable-HashTableTop)

(define-type Inexact-Real-Inf (U +inf.0 +inf.f -inf.0 -inf.f))
(define-predicate inexact-real-inf? Inexact-Real-Inf)
(define-predicate inexact-real-nan? Inexact-Real-Nan)

(define-type Inexact-Rational (Refine [n : Inexact-Real] (! n (U Inexact-Real-Nan Inexact-Real-Inf))))
(define-predicate inexact-rational? Inexact-Rational)

(define-type JSON-Number (U Integer Inexact-Rational JSON-Inf))
(define-predicate json-number? JSON-Number)

(define-type JSON-Inf (U JSON-Pos-Inf JSON-Neg-Inf))
(define-predicate JSON-inf? JSON-Inf)

(define-type JSON-Constant (U JSON-Number JSON-Null Boolean String))
(define-predicate json-constant? JSON-Constant)

(define-type JSON-MList (MListof Mutable-JSON))
(define-type JSON-List  (Listof  Immutable-JSON))
(define-predicate json-list? JSON-List)

(define-type JSON-MHash (Mutable-HashTable   Symbol Mutable-JSON))
(define-type JSON-Hash  (Immutable-HashTable Symbol Immutable-JSON))
(define-predicate json-hash? JSON-Hash)

(define-type Mutable-JSON   (U JSON-Constant JSON-MList JSON-MHash))
(define-type Immutable-JSON (U JSON-Constant JSON-List  JSON-Hash))
(define-type JSON (U Mutable-JSON Immutable-JSON))
(define-predicate immutable-json? Immutable-JSON)
(: json? [-> Any Boolean : JSON])
(define (json? arg)
  (or (immutable-json? arg)
      (mutable-json? arg)))

(define-type JSExpr Any)

(define-type Encode (U 'control 'all))
(define-predicate encode? Encode)


(: mjson? [-> JSON Boolean : Mutable-JSON])
(define (mjson? js)
  (or (json-constant? js)
      (json-mlist? js)
      (json-mhash? js)))

(: json-mlist? [-> JSON Boolean : JSON-MList])
(define (json-mlist? js) (or (null? js) (mpair? js)))

(: json-mhash? [-> (U EOF JSON) Boolean : JSON-MHash])
(define (json-mhash? js) (and (not (eof-object? js)) (mhash? js)))
