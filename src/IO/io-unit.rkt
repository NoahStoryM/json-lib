#lang racket/base

(require racket/unit
         "../types.rkt"
         "../untyped-help.rkt"
         "../Custom/custom-sig.rkt"
         "../JSON/json-sig.rkt"
         "io-sig.rkt")

(require syntax/readerr)

(provide io@)


(define-unit io@
  (import custom^ json^)
  (export io^)

  (define write-JSON*
    (let ()
      (define undefined (gensym 'undefined))

      (λ (who js o
          #:encode  [enc 'control]
          #:format? [format? #f]
          #:indent  [indent  "\t"])
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

        (define rx-to-encode
          (case enc
            ;; FIXME: This should also encode (always) anything that is represented
            ;; with a \U in Racket (since the json thing should be two \u sequences,
            ;; so there should never be a \U in the output of this function); but I
            ;; don't know if there's a known specification to what gets a \U
            [(control) #rx"[\0-\37\\\"\177]"]
            [(all)     #rx"[\0-\37\\\"\177-\U10FFFF]"]))

        (define (write-JSON-string str)
          (write-bytes #"\"" o)
          (write-string (regexp-replace* rx-to-encode str escape) o)
          (write-bytes #"\"" o))

        (define (format/write-whitespace)
          (when format? (write-bytes #" " o)))

        (define (format/write-newline)
          (when format? (write-bytes #"\n" o)))

        (define (format/write-indent layer)
          (when format?
            (for ([i (in-range 0 layer)])
              (write-string indent o))))

        (define (format/write-indentln layer)
          (when format? (format/write-indent layer) (newline o)))

        (define (format/write-lnindent layer)
          (when format? (newline o) (format/write-indent layer)))


        (let loop ([js js] [layer 0])
          (cond
            [(json-constant? js)
             (cond
               [(and (JSON-inf+? js)
                     ;; eliminate endless loops
                     (not (equal? json-inf+ (json-inf+)))
                     (not (JSON-inf+? (json-inf+))))
                (define jsinf+ (json-inf+))
                (parameterize ([json-inf+ undefined])
                  (loop (jsexpr->json jsinf+ #:mutable? #f) layer))]
               [(and (JSON-inf-? js)
                     ;; eliminate endless loops
                     (not (equal? json-inf- (json-inf-)))
                     (not (JSON-inf-? (json-inf-))))
                (define jsinf- (json-inf-))
                (parameterize ([json-inf- undefined])
                  (loop (jsexpr->json jsinf- #:mutable? #f) layer))]
               [(JSON-null? js) (write-bytes #"null"  o)]
               [(eq? js #f)     (write-bytes #"false" o)]
               [(eq? js #t)     (write-bytes #"true"  o)]
               [(or (exact-integer? js) (inexact-rational? js)) (write js o)]
               [(string? js) (write-JSON-string js)]
               [else (raise-type-error who "legal JSON value" js)])]
            [(or (list? js) (mpair? js))
             (write-bytes #"[" o)
             (cond [(pair? js)
                    (loop (car js) (add1 layer))
                    (for ([js (in-list (cdr js))])
                      (write-bytes #"," o)
                      (format/write-whitespace)
                      (loop js (add1 layer)))]
                   [(mpair? js)
                    (loop (mcar js) (add1 layer))
                    (for ([js (in-mlist (mcdr js))])
                      (write-bytes #"," o)
                      (format/write-whitespace)
                      (loop js (add1 layer)))])
             (write-bytes #"]" o)]
            [(hash? js)
             (define first? #t)
             (define (write-hash-kv layer)
               (lambda (k v)
                 (if first? (set! first? #f) (write-bytes #"," o))
                 (format/write-lnindent layer)
                 ;; use a string encoding so we get the same deal with
                 ;; `rx-to-encode'
                 (write-JSON-string (symbol->string k))
                 (write-bytes #":" o)
                 (format/write-whitespace)
                 (loop v layer)))

             (format/write-lnindent layer)
             (write-bytes #"{" o)
             (if (json-hash? js)
                 (hash-for-each js
                                (write-hash-kv (add1 layer))
                                ;; order output
                                #t)
                 (hash-for-each js
                                (write-hash-kv (add1 layer))
                                ;; order output
                                #t))
             (format/write-lnindent layer)
             (write-bytes #"}" o)]))

        (void))))

  (define read-JSON*
    (let ()
      (define (json-whitespace? arg)
        (ormap (λ (ch) (eqv? arg ch))
               '(#\space #\tab #\newline #\return)))

      (define (expt-mark? arg)
        (ormap (λ (b) (eqv? arg b))
               '(#"E" #"e")))

      (define (digit-byte? arg)
        (ormap (λ (n) (eqv? arg n))
               '(#x30 #x31 #x32 #x33 #x34 #x35 #x36 #x37 #x38 #x39)))

      (define (byte-char=? b ch) (eqv? b (char->integer ch)))

      (define (to-json-number n
                              #:inf+ [jsinf+ (json-inf+)]
                              #:inf- [jsinf- (json-inf-)])
        (parameterize ([json-inf+ jsinf+]
                       [json-inf- jsinf-])
          (cond
            [(or (eqv? n +inf.0) (eqv? n (json-inf+))) JSON-inf+]
            [(or (eqv? n -inf.0) (eqv? n (json-inf-))) JSON-inf-]
            [(json-number? n) n]
            [(inexact-real-nan? n) (raise-type-error 'to-json-number "json-number?" n)])))

      (define (to-number c) (- c (char->integer #\0)))

      (define (maybe-bytes c)
        (if (eof-object? c) #"" (bytes c)))

      ;; evaluate n * 10^exp to inexact? without passing large arguments to expt
      ;; assumes n is an integer
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
      (define (n->string n exp)
        (define s (number->string n))
        (string->bytes/utf-8
         (cond
           [(zero? exp) s]
           [else
            (define m (+ (string-length s) exp))
            (string-append (substring s 0 m) "." (substring s m))])))


      (λ (who i #:mutable? mutable?)
        ;; Follows the specification (eg, at json.org) -- no extensions.
        ;;
        (define (err fmt . args)
          (define-values [l c p] (port-next-location i))
          (raise-read-error (format "~a: ~a" who (apply format fmt args))
                            (object-name i) l c p #f))

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

        ;;
        ;; Reading a string *could* have been nearly trivial using the racket
        ;; reader, except that it won't handle a "\/"...
        (define read-a-string
          (let ()
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
                 (define cvtr (or converter (bytes-open-converter "UTF-8" "UTF-8")))
                 (define buf (make-bytes 6 c))

                 (let utf8-loop ([start 0] [end 1])
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

            (define read-escape
              (let ()
                (define get-hex
                  (let ()
                    (define (read-next)
                      (define c (read-byte i))
                      (when (eof-object? c) (error "unexpected end-of-file"))
                      c)

                    (λ ()
                      (define c1 (read-next))
                      (define c2 (read-next))
                      (define c3 (read-next))
                      (define c4 (read-next))

                      (define (hex-convert c)
                        (cond
                          [(<= (char->integer #\0) c (char->integer #\9))
                           (- c (char->integer #\0))]
                          [(<= (char->integer #\a) c (char->integer #\f))
                           (- c (- (char->integer #\a) 10))]
                          [(<= (char->integer #\A) c (char->integer #\F))
                           (- c (- (char->integer #\A) 10))]
                          [else (err "bad \\u escape ~e" (bytes c1 c2 c3 c4))]))

                      (+ (arithmetic-shift (hex-convert c1) 12)
                         (arithmetic-shift (hex-convert c2) 8)
                         (arithmetic-shift (hex-convert c3) 4)
                         (hex-convert c4)))))

                (λ (esc result pos converter)
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
                     (define e (get-hex))
                     (define e*
                       (cond
                         [(<= #xD800 e #xDFFF)
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
                    [else (err "bad string escape: \"~a\"" esc)]))))

            (λ ()
              ;; Using a string output port would make sense here, but managing
              ;; a string buffer directly is even faster
              (define result (make-string 16))

              (loop result 0 #f))))

        ;;
        (define-values (read-list read-mlist)
          (let ()
            (define ((make tcons treverse) what end read-one)
              (define ch (skip-whitespace))
              (cond
                [(eqv? end ch) (read-byte i) '()]
                [else
                 (let loop ([l (tcons (read-one) '())])
                   (define ch (skip-whitespace))

                   (cond
                     [(eqv? ch end) (read-byte i) (treverse l)]
                     [(eqv? ch #\,) (read-byte i) (loop (tcons (read-one) l))]
                     [else
                      (read-byte i) ;; consume the eof
                      (err "error while parsing a json ~a" what)]))]))

            (values (make  cons  reverse)
                    (make mcons mreverse))))

        ;;
        (define-values (read-hash read-mhash)
          (let ()
            (define-values (read-pair read-mpair)
              (let ()
                (define ((make tcons #:mutable? mutable?))
                  (define k (read-JSON #:mutable? mutable?))
                  (unless (string? k) (err "non-string value used for json object key"))

                  (define ch (skip-whitespace))
                  (when (eof-object? ch)
                    (read-byte i) ;; consume the eof
                    (err "unexpected end-of-file while parsing a json object pair"))
                  (unless (char=? #\: ch)
                    (err "error while parsing a json object pair"))
                  (read-byte i)

                  (tcons (string->symbol k) (read-JSON #:mutable? mutable?)))

                (values (make cons #:mutable? #f) (make mcons #:mutable? #t))))

            (values (λ ()
                      (for/hasheq
                          ([p (in-list (read-list 'object #\} read-pair))])
                        (values (car p) (cdr p))))
                    (λ ()
                      (define result (make-hasheq))
                      (for ([p (in-mlist (read-mlist 'object #\} read-mpair))])
                        (hash-set! result (mcar p) (mcdr p)))
                      result))))

        ;;
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
        (define (read-number ch)
          ;; match #rx#"^-?(?:0|[1-9][0-9]*)(?:\\.[0-9]+)?(?:[eE][+-]?[0-9]+)?"
          (define (start)
            (cond
              [(eqv? ch #\-)
               (read-byte i)
               (read-integer -1)]
              [else
               (read-integer 1)]))

          ;; need at least one digit:
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
                (read-exponent (* sgn n) (bytes c) 0)]
               [else (* sgn n)])))

          ;; need at least one digit:
          (define (read-fraction sgn n)
            (define c (read-byte i))
            (cond
              [(digit-byte? c)
               (read-fraction-rest sgn (+ (* n 10) (to-number c)) -1)]
              [else (bad-input (bytes-append (string->bytes/utf-8 (format "~a." (* sgn n)))
                                             (maybe-bytes c))
                               #:eof? (eof-object? c))]))

          ;; more digits:
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
                (read-exponent (* sgn n) (bytes c) exp)]
               [else (exact->inexact (* sgn n (expt 10 exp)))])))

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
          (define (read-exponent-rest n exp exp2 sgn)
            (define c (peek-byte i))
            (cond
              [(digit-byte? c)
               (read-byte i)
               (read-exponent-rest n exp (+ (* 10 exp2) (to-number c)) sgn)]
              [else (safe-exponential->inexact n (+ exp (* sgn exp2)))]))

          (start))

        ;;
        (define (read-JSON #:mutable? mutable? [top? #f])
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

            [(not mutable?)
             (cond
               [(eqv? ch #\[)
                (read-byte i)
                (read-list 'array #\] (λ () (read-JSON #:mutable? #f)))]
               [(eqv? ch #\{)
                (read-byte i)
                (read-hash)]
               [else (bad-input)])]
            [mutable?
             (cond
               [(eqv? ch #\[)
                (read-byte i)
                (read-mlist 'array #\] (λ () (read-JSON #:mutable? #t)))]
               [(eqv? ch #\{)
                (read-byte i)
                (read-mhash)]
               [else (bad-input)])]))

        ;;
        (define (bad-input [prefix #""] #:eof? [eof? #f])
          (define bstr (make-bytes (sub1 (error-print-width))))
          (define bytes-read (peek-bytes-avail!* bstr 0 #f i))
          (if (or (and (eof-object? bytes-read) (equal? prefix #""))
                  eof?)
              (err (string-append "unexpected end-of-file"
                                  (if (equal? prefix #"")
                                      ""
                                      (format "after ~e" prefix))))
              (err (format "bad input starting ~e"
                           (bytes-append prefix
                                         (if (number? bytes-read)
                                             (subbytes bstr 0 bytes-read)
                                             #""))))))

        ;;
        (read-JSON #t #:mutable? mutable?)))))
