#lang typed/racket/base

;; Roughly based on the PLaneT package by Dave Herman,
;;   Originally released under MIT license.

;; edited:
;; -- Matthias, organization in preparation for pretty-print
;; -- Matthias, contracts

;; -----------------------------------------------------------------------------
;; DEPENDENCIES

(require/typed syntax/readerr
  [raise-read-error [-> String Any
                        (U Exact-Positive-Integer False)
                        (U Exact-Nonnegative-Integer False)
                        (U Exact-Positive-Integer False)
                        (U Exact-Nonnegative-Integer False)
                        [#:extra-srclocs (Listof srcloc)]
                        Nothing]])

;; tests in:
;; ~plt/pkgs/racket-test/tests/json/

;; docs in:
;; ~plt/pkgs/racket-doc/json/

;; -----------------------------------------------------------------------------
;; SERVICES

(provide
 ;; Parameter
 json-null json-inf+ json-inf- jsexpr-mutable?

 ;; Type and Predicate
 JSON json? JSExpr jsexpr?

 JS-Inf     js-inf?  js-inf-val
 JS-Pos-Inf js-inf+? JSON-inf+
 JS-Neg-Inf js-inf-? JSON-inf-

 JS-Null    js-null? js-null-val JSON-null

 ;; IO
 write-JSON read-JSON

 write-jsexpr (rename-out [write-jsexpr write-json])
 read-jsexpr  (rename-out [read-jsexpr  read-json])

 ;; Convenience Functions
 json->jsexpr jsexpr->json

 json->string string->json
 json->bytes  bytes->json

 jsexpr->string string->jsexpr
 jsexpr->bytes  bytes->jsexpr
 )

;; -----------------------------------------------------------------------------
;; TYPE and PREDICATE

(define-type Inexact-Real-Inf (U +inf.0 +inf.f -inf.0 -inf.f))
(define-predicate inexact-real-inf? Inexact-Real-Inf)
(define-predicate inexact-real-nan? Inexact-Real-Nan)

(define-type Inexact-Rational (Refine [n : Inexact-Real] (! n (U Inexact-Real-Nan Inexact-Real-Inf))))
(define-predicate inexact-rational? Inexact-Rational)

(define-type JS-Number (U Integer Inexact-Rational JS-Inf))
(define-predicate json-number? JS-Number)

(struct js-inf ([val : (Parameter JSExpr)]) #:transparent #:type-name JS-Inf)
(struct js-inf+ JS-Inf () #:transparent #:type-name JS-Pos-Inf)
(struct js-inf- JS-Inf () #:transparent #:type-name JS-Neg-Inf)

(struct js-null ([val : (Parameter JSExpr)]) #:transparent #:type-name JS-Null)

(define-type JS-List (Listof JSON))
(define-predicate json-list? JS-List)

(define-type JS-Object (Immutable-HashTable Symbol JSON))
(define-predicate json-object? JS-Object)

(define-type JSON (U JS-Number Boolean
                     JS-Null   String
                     JS-List   JS-Object))
(define-predicate json? JSON)

(define-type JSExpr Any)
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
      (or (eq? x json-inf+) (eq? x (json-inf+))
          (eq? x json-inf-) (eq? x (json-inf-))
          (eq? x json-null) (eq? x (json-null))
          (js-null? x)
          (json-number? x)
          (boolean? x)
          (string? x)
          (and (list? x) (andmap loop x))
          (and (hash? x) (for/and ([(k v) (in-hash x)])
                           (and (symbol? k) (loop v))))))))


(define-type Encode (U 'control 'all))

;; -----------------------------------------------------------------------------
;; CUSTOMIZATION

;; The default translation for a JSON `null' value
(: json-null (Parameter JSExpr))
(define json-null (make-parameter 'null))

(: JSON-null JS-Null)
(define JSON-null (js-null json-null))


;; The default translation for a Racket `+inf' value
(: json-inf+ (Parameter JSExpr))
(define json-inf+ (make-parameter +inf.0))

(: JSON-inf+ JS-Pos-Inf)
(define JSON-inf+ (js-inf+ json-inf+))


;; The default translation for a Racket `-inf' value
(: json-inf- (Parameter JSExpr))
(define json-inf- (make-parameter -inf.0))

(: JSON-inf- JS-Neg-Inf)
(define JSON-inf- (js-inf- json-inf-))


(: jsexpr-mutable? (Parameter Boolean))
(define jsexpr-mutable? (make-parameter #f))

;; -----------------------------------------------------------------------------
;; GENERATION  (from Racket to JSON)

(: write-jsexpr [->* (JSON)
                     (Output-Port
                      Symbol
                      #:null JSExpr
                      #:inf+ JSExpr
                      #:inf- JSExpr
                      #:encode Encode)
                     Void])
(define (write-jsexpr x
                      [o (current-output-port)]
                      [who 'write-jsexpr]
                      #:null [jsnull (json-null)]
                      #:inf+ [jsinf+ (json-inf+)]
                      #:inf- [jsinf- (json-inf-)]
                      #:encode [enc 'control])
  (parameterize ([json-null jsnull]
                 [json-inf+ jsinf+]
                 [json-inf- jsinf-])
    (write-JSON* who (jsexpr->json x) o enc)))

(: write-JSON [->* (JSON)
                   (Output-Port
                    Symbol
                    #:encode Encode)
                   Void])
(define (write-JSON js
                    [o (current-output-port)]
                    [who 'write-JSON]
                    #:encode [enc 'control])
  (write-JSON* who js o enc))

(: write-JSON* [-> Symbol JSON Output-Port Encode Void])
(define write-JSON*
  (let ()
    (define undefined (gensym 'undefined))

    (λ (who js o enc)
      (: escape [-> String String * String])
      (define (escape m . ms)
        (define ch (string-ref m 0))
        (case ch
          [(#\backspace) "\\b"]
          [(#\newline) "\\n"]
          [(#\return) "\\r"]
          [(#\page) "\\f"]
          [(#\tab) "\\t"]
          [(#\\) "\\\\"]
          [(#\") "\\\""]
          [else
           (: u-esc [-> Number String])
           (define (u-esc n)
             (define str (number->string n 16))
             (define pad
               (case (string-length str)
                 [(1) "000"]
                 [(2) "00"]
                 [(3) "0"]
                 [else ""]))
             (string-append "\\u" pad str))

           (define n (char->integer ch))

           (if (n . < . #x10000)
               (u-esc n)
               ;; use the (utf-16 surrogate pair) double \u-encoding
               (let ([n (- n #x10000)])
                 (string-append (u-esc (+ #xD800 (arithmetic-shift n -10)))
                                (u-esc (+ #xDC00 (bitwise-and n #x3FF))))))]))

      (: rx-to-encode Regexp)
      (define rx-to-encode
        (case enc
          ;; FIXME: This should also encode (always) anything that is represented
          ;; with a \U in Racket (since the json thing should be two \u sequences,
          ;; so there should never be a \U in the output of this function); but I
          ;; don't know if there's a known specification to what gets a \U
          [(control) #rx"[\0-\37\\\"\177]"]
          [(all)     #rx"[\0-\37\\\"\177-\U10FFFF]"]))

      (: write-JSON-string [-> String Index])
      (define (write-JSON-string str)
        (write-bytes #"\"" o)
        (write-string (regexp-replace* rx-to-encode str escape) o)
        (write-bytes #"\"" o))


      (let loop : Any ([js js])
        (cond
          [(and (js-inf+? js)
                ;; eliminate endless loops
                (not (eq? (json-inf+) json-inf+))
                (not (js-inf+? (json-inf+))))
           (define jsinf+ (json-inf+))
           (parameterize ([json-inf+ undefined])
             (loop (jsexpr->json jsinf+)))]
          [(and (js-inf-? js)
                ;; eliminate endless loops
                (not (eq? (json-inf-) json-inf-))
                (not (js-inf-? (json-inf-))))
           (define jsinf- (json-inf-))
           (parameterize ([json-inf- undefined])
             (loop (jsexpr->json jsinf-)))]
          [(js-null? js) (write-bytes #"null"  o)]
          [(eq? js #f)   (write-bytes #"false" o)]
          [(eq? js #t)   (write-bytes #"true"  o)]
          [(or (exact-integer? js) (inexact-rational? js)) (write js o)]
          [(string? js) (write-JSON-string js)]
          [(json-list? js)
           (write-bytes #"[" o)
           (when (pair? js)
             (loop (car js))
             (for ([js (in-list (cdr js))])
               (write-bytes #"," o)
               (loop js)))
           (write-bytes #"]" o)]
          [(json-object? js)
           (: first? Boolean)
           (define first? #t)

           (write-bytes #"{" o)
           (hash-for-each
            js
            (ann (lambda (k v)
                   (if first? (set! first? #f) (write-bytes #"," o))
                   ;; use a string encoding so we get the same deal with
                   ;; `rx-to-encode'
                   (write-JSON-string (symbol->string k))
                   (write-bytes #":" o)
                   (loop v))
                 [-> Symbol JSON Any])
            ;; order output
            #t)
           (write-bytes #"}" o)]
          [else (raise-type-error who "legal JSON value" js)]))

      (void))))

;; -----------------------------------------------------------------------------
;; PARSING (from JSON to Racket)

(: read-jsexpr [->* ()
                    (Input-Port
                     Symbol
                     #:null JSExpr
                     #:inf+ JSExpr
                     #:inf- JSExpr
                     #:mutable? Boolean)
                    JSExpr])
(define (read-jsexpr [i (current-input-port)]
                     [who 'read-jsexpr]
                     #:null [jsnull (json-null)]
                     #:inf+ [jsinf+ (json-inf+)]
                     #:inf- [jsinf- (json-inf-)]
                     #:mutable? [mutable? (jsexpr-mutable?)])
  (parameterize ([json-null jsnull]
                 [json-inf+ jsinf+]
                 [json-inf- jsinf-]
                 [jsexpr-mutable? mutable?])
    (define js (read-JSON* who i))
    (if (eof-object? js)
        eof
        (json->jsexpr js))))

(: read-JSON [->* () (Input-Port Symbol) (U EOF JSON)])
(define (read-JSON [i (current-input-port)] [who 'read-JSON])
  (read-JSON* who i))

(: read-JSON* [-> Symbol Input-Port (U EOF JSON)])
(define read-JSON*
  (let ()
    (define-type JS-Whitespace (U #\space #\tab #\newline #\return))
    (define-predicate json-whitespace? JS-Whitespace)

    (define-type SGN (U -1 1))
    (define-type SGN-Mark  (U #"-" #"+"))
    (define-type Expt-Mark (U #"E" #"e"))
    (define-predicate expt-mark? Expt-Mark)

    (define-type Digit-Byte (U #x30 #x31 #x32 #x33 #x34 #x35 #x36 #x37 #x38 #x39))
    (define-predicate digit-byte? Digit-Byte)

    (λ (who i)
      ;; Follows the specification (eg, at json.org) -- no extensions.
      ;;
      (: err [-> String Any * Nothing])
      (define (err fmt . args)
        (define-values [l c p] (port-next-location i))
        (raise-read-error (format "~a: ~a" who (apply format fmt args))
                          (object-name i) l c p #f))

      (: skip-whitespace [-> (U Char EOF)])
      (define (skip-whitespace)
        (define ch (peek-char i))
        (cond
          [(char? ch)
           (cond
             [(json-whitespace? ch)
              (read-char i)
              (skip-whitespace)]
             [(char-whitespace? ch)
              (err "found whitespace that is not allowed by the JSON specification\n  char: ~s" ch)]
             [else ch])]
          [else ch]))

      (: byte-char=? [-> Byte Char Boolean])
      (define (byte-char=? b ch) (eqv? b (char->integer ch)))

      ;;
      ;; Reading a string *could* have been nearly trivial using the racket
      ;; reader, except that it won't handle a "\/"...
      (: read-a-string [-> String])
      (define (read-a-string)
        ;; Using a string output port would make sense here, but managing
        ;; a string buffer directly is even faster
        (define result (make-string 16))

        (: keep-char [-> Char String Natural (Option Bytes-Converter) String])
        (define (keep-char c old-result pos converter)
          (define result
            (cond
              [(= pos (string-length old-result))
               (define new (make-string (* pos 2)))
               (string-copy! new 0 old-result 0 pos)
               new]
              [else old-result]))
          (string-set! result pos c)
          (loop result (add1 pos) converter))

        (: loop [-> String Natural (Option Bytes-Converter) String])
        (define (loop result pos converter)
          (define c (read-byte i))
          (cond
            [(eof-object? c) (err "unterminated string")]
            [(byte-char=? c #\") (substring result 0 pos)]
            [(byte-char=? c #\\)
             (define ch (read-char i))
             (if (eof-object? ch)
                 (err "unexpected end-of-file")
                 (read-escape ch result pos converter))]
            [(c . < . 128) (keep-char (integer->char c) result pos converter)]
            [else
             ;; need to decode, but we can't un-read the byte, and
             ;; also we want to report decoding errors
             (define cvtr (or converter
                              (assert (bytes-open-converter "UTF-8" "UTF-8") bytes-converter?)))
             (define buf (make-bytes 6 c))

             (let utf8-loop : String ([start : Natural 0] [end : Natural 1])
               (define-values (wrote-n read-n state)
                 (bytes-convert cvtr buf start end buf 0 6))

               (case state
                 [(complete)
                  (keep-char (bytes-utf-8-ref buf 0) result pos cvtr)]
                 [(error)
                  (err "UTF-8 decoding error at ~e" (subbytes buf 0 end))]
                 [(continues)
                  (err "no enough space at ~e" buf)]
                 [(aborts)
                  (define c (read-byte i))
                  (cond
                    [(eof-object? c)
                     (err "unexpected end-of-file")]
                    [else
                     (bytes-set! buf end c)
                     (utf8-loop (+ start read-n) (add1 end))])]))]))

        (: read-escape [-> Char String Natural (Option Bytes-Converter) String])
        (define (read-escape esc result pos converter)
          (cond
            [(case esc
               [(#\b) "\b"]
               [(#\n) "\n"]
               [(#\r) "\r"]
               [(#\f) "\f"]
               [(#\t) "\t"]
               [(#\\) "\\"]
               [(#\") "\""]
               [(#\/) "/"]
               [else #f])
             => (λ (s) (keep-char (string-ref s 0) result pos converter))]
            [(eqv? esc #\u)

             (: get-hex [-> Natural])
             (define (get-hex)
               (: read-next [-> Byte])
               (define (read-next)
                 (define c (read-byte i))
                 (when (eof-object? c) (error "unexpected end-of-file"))
                 c)

               (define c1 (read-next))
               (define c2 (read-next))
               (define c3 (read-next))
               (define c4 (read-next))

               (: hex-convert [-> Byte Byte])
               (define (hex-convert c)
                 (assert
                  (cond
                    [(<= (char->integer #\0) c (char->integer #\9))
                     (- c (char->integer #\0))]
                    [(<= (char->integer #\a) c (char->integer #\f))
                     (- c (- (char->integer #\a) 10))]
                    [(<= (char->integer #\A) c (char->integer #\F))
                     (- c (- (char->integer #\A) 10))]
                    [else (err "bad \\u escape ~e" (bytes c1 c2 c3 c4))])
                  byte?))

               (+ (arithmetic-shift (hex-convert c1) 12)
                  (arithmetic-shift (hex-convert c2) 8)
                  (arithmetic-shift (hex-convert c3) 4)
                  (hex-convert c4)))


             (define e (get-hex))
             (define e*
               (cond
                 [(<= #xD800 e #xDFFF)
                  (: err-missing [-> Nothing])
                  (define (err-missing)
                    (err "bad string \\u escape, missing second half of a UTF-16 pair"))
                  (unless (eqv? (read-byte i) (char->integer #\\)) (err-missing))
                  (unless (eqv? (read-byte i) (char->integer #\u)) (err-missing))

                  (define e2 (get-hex))
                  (cond
                    [(<= #xDC00 e2 #xDFFF)
                     (+ (arithmetic-shift (- e #xD800) 10) (- e2 #xDC00) #x10000)]
                    [else
                     (err "bad string \\u escape, bad second half of a UTF-16 pair")])]
                 [else e]))

             (keep-char (integer->char e*) result pos converter)]
            [else (err "bad string escape: \"~a\"" esc)]))

        (loop result 0 #f))

      ;;
      (: read-list (All (A) [-> Symbol Char [-> A] (Listof A)]))
      (define (read-list what end read-one)
        (define ch (skip-whitespace))
        (cond
          [(eqv? end ch) (read-byte i) '()]
          [else
           (let loop : (Listof A) ([l : (Listof A) (list (read-one))])
             (define ch (skip-whitespace))

             (cond
               [(eqv? ch end) (read-byte i) (reverse l)]
               [(eqv? ch #\,) (read-byte i) (loop (cons (read-one) l))]
               [else
                (read-byte i) ;; consume the eof
                (err "error while parsing a json ~a" what)]))]))

      ;;
      (: read-hash [-> (Immutable-HashTable Symbol JSON)])
      (define (read-hash)
        (: read-pair [-> (Pair Symbol JSON)])
        (define (read-pair)
          (define k (read-JSON))
          (unless (string? k) (err "non-string value used for json object key"))
          (define ch (skip-whitespace))
          (when (eof-object? ch)
            (read-byte i) ;; consume the eof
            (err "unexpected end-of-file while parsing a json object pair"))
          (unless (char=? #\: ch)
            (err "error while parsing a json object pair"))
          (read-byte i)
          (cons (string->symbol k) (read-JSON)))

        (for/hasheq : (Immutable-HashTable Symbol JSON)
             ([p (in-list (read-list 'object #\} read-pair))])
          (values (car p) (cdr p))))

      ;;
      (: read-literal [-> Bytes Void])
      (define (read-literal bstr)
        (define len (bytes-length bstr))
        (read-byte i)
        (for ([j (in-range 1 len)])
          (define c (read-byte i))
          (when (eof-object? c)
            (read-byte i) ;; consume the eof
            (err "unexpected end-of-file while parsing a json literal"))
          (unless (eqv? c (bytes-ref bstr j))
            (bad-input (bytes-append (subbytes bstr 0 j) (bytes c)))))

        ;; Check for delimiter, defined for our purposes as matching #rx"\\b":
        (define b (peek-byte i))
        (unless (eof-object? b)
          (when (or (<= (char->integer #\a) b (char->integer #\z))
                    (<= (char->integer #\A) b (char->integer #\Z))
                    (<= (char->integer #\0) b (char->integer #\9))
                    (eqv? b (char->integer #\_)))
            (bad-input bstr))))

      ;;
      (: read-number [-> Char JS-Number])
      (define (read-number ch)
        ;; match #rx#"^-?(?:0|[1-9][0-9]*)(?:\\.[0-9]+)?(?:[eE][+-]?[0-9]+)?"
        (: start [-> JS-Number])
        (define (start)
          (cond
            [(eqv? ch #\-)
             (read-byte i)
             (read-integer -1)]
            [else
             (read-integer 1)]))

        (: to-number [-> Digit-Byte Byte])
        (define (to-number c)
          (assert (- c (char->integer #\0)) byte?))

        (: maybe-bytes [-> (U EOF Byte) Bytes])
        (define (maybe-bytes c)
          (if (eof-object? c) #"" (bytes c)))

        ;; evaluate n * 10^exp to inexact? without passing large arguments to expt
        ;; assumes n is an integer
        (: safe-exponential->inexact [-> Integer Integer JS-Number])
        (define (safe-exponential->inexact n exp)
          (define result-exp
            (if (= n 0)
                exp
                (+ (log (abs n) 10) exp)))

          (to-json-number
           (cond
             [(< result-exp -400)
              (cond
                [(>= n 0) 0.0]
                [else -0.0])]
             [(> result-exp 400)
              (cond
                [(= n 0) 0.0]
                [(> n 0) +inf.0]
                [(< n 0) -inf.0])]
             [else
              (exact->inexact (* n (expt 10 exp)))])))

        ;; used to reconstruct input for error reporting:
        (: n->string [-> Integer Integer Bytes])
        (define (n->string n exp)
          (define s (number->string n))
          (string->bytes/utf-8
           (cond
             [(zero? exp) s]
             [else
              (define m (+ (string-length s) exp))
              (string-append (substring s 0 m) "." (substring s m))])))

        ;; need at least one digit:
        (: read-integer [-> SGN JS-Number])
        (define (read-integer sgn)
          (define c (read-byte i))
          (cond
            [(digit-byte? c)
             (read-integer-rest sgn (to-number c)
                                #:more-digits? (not (eqv? c (char->integer #\0))))]
            [else (bad-input (bytes-append (if (sgn . < . 0) #"-" #"")
                                           (maybe-bytes c))
                             #:eof? (eof-object? c))]))

        ;; more digits:
        (: read-integer-rest [-> SGN Byte #:more-digits? Boolean JS-Number])
        (define (read-integer-rest sgn n #:more-digits? more-digits?)
          (define c (peek-byte i))
          (to-json-number
           (cond
             [(and more-digits? (digit-byte? c))
              (read-byte i)
              (read-integer-rest sgn (+ (* n 10) (to-number c)) #:more-digits? #t)]
             [(eqv? c (char->integer #\.))
              (read-byte i)
              (read-fraction sgn n)]
             [(or (eqv? c (char->integer #\e))
                  (eqv? c (char->integer #\E)))
              (read-byte i)
              (read-exponent (* sgn n) (assert (bytes c) expt-mark?) 0)]
             [else (* sgn n)])))

        ;; need at least one digit:
        (: read-fraction [-> SGN Byte JS-Number])
        (define (read-fraction sgn n)
          (define c (read-byte i))
          (cond
            [(digit-byte? c)
             (read-fraction-rest sgn (+ (* n 10) (to-number c)) -1)]
            [else (bad-input (bytes-append (string->bytes/utf-8 (format "~a." (* sgn n)))
                                           (maybe-bytes c))
                             #:eof? (eof-object? c))]))

        ;; more digits:
        (: read-fraction-rest [-> SGN Natural Integer JS-Number])
        (define (read-fraction-rest sgn n exp)
          (define c (peek-byte i))
          (to-json-number
           (cond
             [(digit-byte? c)
              (read-byte i)
              (read-fraction-rest sgn (+ (* n 10) (to-number c)) (sub1 exp))]
             [(or (eqv? c (char->integer #\e))
                  (eqv? c (char->integer #\E)))
              (read-byte i)
              (read-exponent (* sgn n) (assert (bytes c) expt-mark?) exp)]
             [else (exact->inexact (* sgn n (expt 10 exp)))])))

        (: read-exponent [-> Integer Expt-Mark Integer JS-Number])
        ;; need at least one digit, maybe after +/-:
        (define (read-exponent n mark exp)
          (define c (read-byte i))
          (cond
            [(digit-byte? c)
             (read-exponent-rest n exp (to-number c) 1)]
            [(eqv? c (char->integer #\+))
             (read-exponent-more n mark #"+" exp 1)]
            [(eqv? c (char->integer #\-))
             (read-exponent-more n mark #"-" exp -1)]
            [else (bad-input (bytes-append (n->string n exp)
                                           mark
                                           (maybe-bytes c))
                             #:eof? (eof-object? c))]))

        ;; need at least one digit, still:
        (: read-exponent-more [-> Integer Expt-Mark SGN-Mark Integer SGN JS-Number])
        (define (read-exponent-more n mark mark2 exp sgn)
          (define c (read-byte i))
          (cond
            [(digit-byte? c)
             (read-exponent-rest n exp (to-number c) sgn)]
            [else (bad-input (bytes-append (n->string n exp)
                                           mark
                                           mark2
                                           (maybe-bytes c))
                             #:eof? (eof-object? c))]))

        ;; more digits:
        (: read-exponent-rest [-> Integer Integer Integer SGN JS-Number])
        (define (read-exponent-rest n exp exp2 sgn)
          (define c (peek-byte i))
          (cond
            [(digit-byte? c)
             (read-byte i)
             (read-exponent-rest n exp (+ (* 10 exp2) (to-number c)) sgn)]
            [else (safe-exponential->inexact n (+ exp (* sgn exp2)))]))

        (start))

      ;;
      (: read-JSON (case-> [->* () (False) JSON]
                           [->* () (Boolean) (U EOF JSON)]))
      (define (read-JSON [top? #f])
        (define ch (skip-whitespace))
        (cond
          [(eof-object? ch)
           (read-byte i) ;; consume the eof
           (if top?
               eof
               (bad-input))]
          [(eqv? ch #\t) (read-literal #"true") #t]
          [(eqv? ch #\f) (read-literal #"false") #f]
          [(eqv? ch #\n) (read-literal #"null") JSON-null]
          [(or (and ((char->integer ch) . <= . (char->integer #\9))
                    ((char->integer ch) . >= . (char->integer #\0)))
               (eqv? ch #\-))
           (read-number ch)]
          [(eqv? ch #\") (read-byte i)
                         (read-a-string)]
          [(eqv? ch #\[) (read-byte i)
                         (read-list 'array #\] read-JSON)]
          [(eqv? ch #\{) (read-byte i)
                         (read-hash)]
          [else (bad-input)]))

      ;;
      (: bad-input [->* () (Bytes #:eof? Boolean) Nothing])
      (define (bad-input [prefix #""] #:eof? [eof? #f])
        (define bstr (make-bytes (sub1 (error-print-width))))
        (define bytes-read (peek-bytes-avail!* bstr 0 #f i))
        (if (or (and (eof-object? bytes-read) (equal? prefix #""))
                eof?)
            (err (string-append "unexpected end-of-file"
                                (if (equal? prefix #"")
                                    ""
                                    (format "after ~e" prefix))))
            (err (format "bad input starting ~e" (bytes-append prefix (if (number? bytes-read)
                                                                          (subbytes bstr 0 bytes-read)
                                                                          #""))))))
      ;;
      (read-JSON #t))))

;; -----------------------------------------------------------------------------
;; CONVENIENCE FUNCTIONS

(: to-json-number [-> (U JS-Number Inexact-Real)
                      [#:inf+ JSExpr]
                      [#:inf- JSExpr]
                      JS-Number])
(define (to-json-number n
                        #:inf+ [jsinf+ (json-inf+)]
                        #:inf- [jsinf- (json-inf-)])
  (parameterize ([json-inf+ jsinf+]
                 [json-inf- jsinf-])
    (cond
      [(or (eq? n +inf.0) (eq? n (json-inf+))) JSON-inf+]
      [(or (eq? n -inf.0) (eq? n (json-inf-))) JSON-inf-]
      [(json-number? n) n]
      [(inexact-real-nan? n) (raise-type-error 'to-json-number "json-number?" n)])))


(: json->jsexpr [-> JSON
                    [#:null JSExpr]
                    [#:inf+ JSExpr]
                    [#:inf- JSExpr]
                    [#:mutable? Boolean]
                    JSExpr])
(define (json->jsexpr js
                      #:null [jsnull (json-null)]
                      #:inf+ [jsinf+ (json-inf+)]
                      #:inf- [jsinf- (json-inf-)]
                      #:mutable? [mutable? (jsexpr-mutable?)])
  (parameterize ([json-null jsnull]
                 [json-inf+ jsinf+]
                 [json-inf- jsinf-]
                 [jsexpr-mutable? mutable?])
    (cond
      [(json-number? js)
       (cond [(or (exact-integer? js) (inexact-rational? js)) js]
             [(js-inf+? js) (json-inf+)]
             [(js-inf-? js) (json-inf-)])]
      [(js-null? js) (json-null)]
      [(boolean? js) js]
      [(string? js) js]
      [(json-list? js) (map json->jsexpr js)]
      [(json-object? js)
       (cond
         [mutable?
          (: result (Mutable-HashTable Symbol JSExpr))
          (define result (make-hasheq))
          (for ([(k v) (in-hash js)])
            (hash-set! result k v))
          result]
         [else
          (for/hasheq : (Immutable-HashTable Symbol JSExpr)
               ([(k v) (in-hash js)])
            (values k (json->jsexpr v)))])])))

(: jsexpr->json [-> JSExpr
                    [#:null JSExpr]
                    [#:inf+ JSExpr]
                    [#:inf- JSExpr]
                    JSON])
(define (jsexpr->json x
                      #:null [jsnull (json-null)]
                      #:inf+ [jsinf+ (json-inf+)]
                      #:inf- [jsinf- (json-inf-)])
  (parameterize ([json-null jsnull]
                 [json-inf+ jsinf+]
                 [json-inf- jsinf-])
    (cond
      [(or (eq? x json-inf+) (eq? x (json-inf+)) (js-inf+? x)) JSON-inf+]
      [(or (eq? x json-inf-) (eq? x (json-inf-)) (js-inf-? x)) JSON-inf-]
      [(or (eq? x json-null) (eq? x (json-null)) (js-null? x)) JSON-null]
      [(boolean? x) x]
      [(json-number? x) x]
      [(string? x) x]
      [(list? x) (map jsexpr->json x)]
      [(hash? x)
       (for/hasheq : (Immutable-HashTable Symbol JSON)
            ([(k v) (in-hash x)])
         (values (assert k symbol?) (jsexpr->json v)))]
      [else (raise-type-error 'jsexpr->json "jsexpr?" x)])))


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

(: string->json [->* (String) (Symbol) (U EOF JSON)])
(define (string->json str [who 'string->json])
  (define i (open-input-string str))
  (read-JSON i who))

(: bytes->json [->* (Bytes) (Symbol) (U EOF JSON)])
(define (bytes->json bs [who 'bytes->json])
  (define i (open-input-bytes bs))
  (read-JSON i who))


(: jsexpr->string [->* (JSExpr)
                       (Symbol
                        #:null JSExpr
                        #:inf+ JSExpr
                        #:inf- JSExpr
                        #:encode  Encode)
                       String])
(define (jsexpr->string x
                        [who 'jsexpr->string]
                        #:null [jsnull (json-null)]
                        #:inf+ [jsinf+ (json-inf+)]
                        #:inf- [jsinf- (json-inf-)]
                        #:encode  [enc 'control])
  (parameterize ([json-null jsnull]
                 [json-inf+ jsinf+]
                 [json-inf- jsinf-])
    (json->string (jsexpr->json x) who #:encode enc)))

(: jsexpr->bytes [->* (JSExpr)
                      (Symbol
                       #:null JSExpr
                       #:inf+ JSExpr
                       #:inf- JSExpr
                       #:encode  Encode)
                      Bytes])
(define (jsexpr->bytes x
                       [who 'jsexpr->bytes]
                       #:null [jsnull (json-null)]
                       #:inf+ [jsinf+ (json-inf+)]
                       #:inf- [jsinf- (json-inf-)]
                       #:encode  [enc 'control])
  (parameterize ([json-null jsnull]
                 [json-inf+ jsinf+]
                 [json-inf- jsinf-])
    (json->bytes (jsexpr->json x) who #:encode enc)))

(: string->jsexpr [->* (String)
                       (Symbol
                        #:null JSExpr
                        #:inf+ JSExpr
                        #:inf- JSExpr
                        #:mutable? Boolean)
                       (U EOF JSExpr)])
(define (string->jsexpr str
                        [who 'string->jsexpr]
                        #:null [jsnull (json-null)]
                        #:inf+ [jsinf+ (json-inf+)]
                        #:inf- [jsinf- (json-inf-)]
                        #:mutable? [mutable? (jsexpr-mutable?)])
  (define i (open-input-string str))
  (parameterize ([json-null jsnull]
                 [json-inf+ jsinf+]
                 [json-inf- jsinf-]
                 [jsexpr-mutable? mutable?])
    (define js (read-JSON* who i))
    (if (eof-object? js)
        eof
        (json->jsexpr js))))

(: bytes->jsexpr [->* (Bytes)
                      (Symbol
                       #:null JSExpr
                       #:inf+ JSExpr
                       #:inf- JSExpr
                       #:mutable? Boolean)
                      (U EOF JSExpr)])
(define (bytes->jsexpr bs
                       [who 'bytes->jsexpr]
                       #:null [jsnull (json-null)]
                       #:inf+ [jsinf+ (json-inf+)]
                       #:inf- [jsinf- (json-inf-)]
                       #:mutable? [mutable? (jsexpr-mutable?)])
  (define i (open-input-bytes bs))
  (parameterize ([json-null jsnull]
                 [json-inf+ jsinf+]
                 [json-inf- jsinf-]
                 [jsexpr-mutable? mutable?])
    (define js (read-JSON* who i))
    (if (eof-object? js)
        eof
        (json->jsexpr js))))
