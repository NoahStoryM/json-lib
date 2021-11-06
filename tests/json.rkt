#lang at-exp racket/base

;; Mathias, added test for contracts on read-jsexpr

(require "../main.rkt" racket/string tests/eli-tester
         racket/port racket/contract)

(define T string-append)

(define (pred-tests)
  (test
   ;; test jsexpr?
   (jsexpr? 0)
   (jsexpr? 1)
   (jsexpr? -1)
   (jsexpr? 12345)
   (jsexpr? 0.0)
   (jsexpr? 1.0)
   (not (jsexpr? 1/2))
   (not (jsexpr? +i))
   (not (jsexpr? 'foo))
   (not (jsexpr? 'true))
   (jsexpr? #t)
   (jsexpr? #f)
   (jsexpr? 'null)
   (jsexpr? "")
   (jsexpr? "abc")
   (jsexpr? "abc\n\\")
   (jsexpr? '())
   (jsexpr? '(1 2 3))
   (jsexpr? '(1 "2" (3) #t #f null))
   (jsexpr? '((((())))))
   (not (jsexpr? '(1 2 . 3)))
   (not (jsexpr? '#(1 2 3)))
   (jsexpr? '#hasheq())
   (jsexpr? '#hasheq([x . 1]))
   (jsexpr? '#hasheq([x . 1] [y . 2]))
   (jsexpr? '#hash([x . 1] [y . 2])) ; fine as a jsexpr too
   (jsexpr? '#hasheq([|x\y| . 1] [y . 2]))
   (not (jsexpr? '#hasheq([1 . 1])))
   (not (jsexpr? '#hasheq(["x" . 1])))
   (not (jsexpr? '#hasheq([() . 1])))
   (jsexpr? (hash-copy '#hasheq()))
   (jsexpr? (hash-copy '#hasheq([x . 1])))
   (jsexpr? (hash-copy '#hasheq([x . 1] [y . 2])))
   (jsexpr? (hash-copy '#hash([x . 1] [y . 2]))) ; fine as a jsexpr too
   (jsexpr? (hash-copy '#hasheq([|x\y| . 1] [y . 2])))
   (not (jsexpr? (hash-copy '#hasheq([1 . 1]))))
   (not (jsexpr? (hash-copy '#hasheq(["x" . 1]))))
   (not (jsexpr? (hash-copy '#hasheq([() . 1]))))
   ;;;; See https://github.com/racket/typed-racket/issues/1161
   ;; (not (jsexpr? '#hasheq(['() . 1])))
   ;; (not (jsexpr? (hash-copy '#hasheq(['() . 1]))))
   (not (jsexpr? (/ 1.0 0.0)))
   (not (jsexpr? (/ -1.0 0.0)))
   (not (jsexpr? (/ 0.0 0.0)))
   (jsexpr? +inf.0)
   (jsexpr? -inf.0)
   (not (jsexpr? +nan.0))
   (jsexpr? +inf.f)
   (jsexpr? -inf.f)
   (not (jsexpr? +nan.f))

   ;; test immutable-json?
   (immutable-json? 0)
   (immutable-json? 1)
   (immutable-json? -1)
   (immutable-json? 12345)
   (immutable-json? 0.0)
   (immutable-json? 1.0)
   (not (immutable-json? 1/2))
   (not (immutable-json? +i))
   (not (immutable-json? 'foo))
   (not (immutable-json? 'true))
   (immutable-json? #t)
   (immutable-json? #f)
   (immutable-json? JSON-null)
   (not (immutable-json? 'null))
   (immutable-json? "")
   (immutable-json? "abc")
   (immutable-json? "abc\n\\")
   (immutable-json? '())
   (immutable-json? '(1 2 3))
   (immutable-json? `(1 "2" (3) #t #f ,JSON-null))
   (not (immutable-json? '(1 "2" (3) #t #f null)))
   (immutable-json? `(1 "2" (3) #t #f ,JSON-null))
   (immutable-json? '((((())))))
   (not (immutable-json? '(1 2 . 3)))
   (not (immutable-json? '#(1 2 3)))
   (immutable-json? '#hasheq())
   (immutable-json? '#hasheq([x . 1]))
   (immutable-json? '#hasheq([x . 1] [y . 2]))
   (immutable-json? '#hash([x . 1] [y . 2])) ; fine as a json too
   (immutable-json? '#hasheq([|x\y| . 1] [y . 2]))
   (mutable-json? (hash-copy '#hasheq()))
   (mutable-json? (hash-copy '#hasheq([x . 1])))
   (mutable-json? (hash-copy '#hasheq([x . 1] [y . 2])))
   (mutable-json? (hash-copy '#hash([x . 1] [y . 2])))
   (mutable-json? (hash-copy '#hasheq([|x\y| . 1] [y . 2])))
   (not (immutable-json? '#hasheq([1 . 1])))
   (not (immutable-json? '#hasheq(["x" . 1])))
   (not (immutable-json? '#hasheq(['() . 1])))
   (not (immutable-json? (hash-copy '#hasheq())))
   (not (immutable-json? (hash-copy '#hasheq([x . 1]))))
   (not (immutable-json? (hash-copy '#hasheq([x . 1] [y . 2]))))
   (not (immutable-json? (hash-copy '#hash([x . 1] [y . 2]))))
   (not (immutable-json? (hash-copy '#hasheq([|x\y| . 1] [y . 2]))))
   (not (immutable-json? (/ 1.0 0.0)))
   (not (immutable-json? (/ -1.0 0.0)))
   (not (immutable-json? (/ 0.0 0.0)))
   (immutable-json? JSON-inf+)
   (immutable-json? JSON-inf-)
   (not (immutable-json? +inf.0))
   (not (immutable-json? -inf.0))
   (not (immutable-json? +nan.0))
   (not (immutable-json? +inf.f))
   (not (immutable-json? -inf.f))
   (not (immutable-json? +nan.f))))

(define (print-tests)
  (for ([x (list 0 1 -1 12345 0.0 1.0 #t #f (λ (n) n) "" "abc" "abc\n\\"
                 '() '(1 2 3) (λ (n) `(1 "2" (3) #t #f ,n)) '((((()))))
                 '#hasheq()
                 '#hasheq([x . 1])
                 '#hasheq([x . 1] [y . 2])
                 '#hasheq([|x\y| . 1] [y . 2])
                 ;; string escapes
                 "λ" "\U1D11E" ; goes as a plain character in normal encoding
                 "\0" "\1" "\2" "\3" "\37" "\177" ; encoded as json \u escapes
                 "\b" "\n" "\r" "\f" "\t"         ; same escapes in both
                 "\a" "\v" "\e"                   ; does not use racket escapes
                 )])
    (define (N x null) (if (procedure? x) (x null) x))
    (test
     ;; test jsexpr
     ;; default
     (string->jsexpr (jsexpr->string (N x 'null)))
     => (N x 'null)
     ;; different null
     (string->jsexpr (jsexpr->string (N x #\null) #:null #\null) #:null #\null)
     => (N x #\null)
     ;; encode all non-ascii
     (string->jsexpr (jsexpr->string (N x 'null) #:encode 'all))
     => (N x 'null)

     ;; test json
     ;; default
     (string->json #:mutable? #f (json->string (N x JSON-null)))
     => (N x JSON-null)
     ;; encode all non-ascii
     (string->json #:mutable? #f (json->string (N x JSON-null) #:encode 'all))
     => (N x JSON-null)))

  ;; also test some specific expected encodings
  (test
   ;; test jsexpr
   (jsexpr->string "\0\1\2\3") => "\"\\u0000\\u0001\\u0002\\u0003\""
   (jsexpr->string "\b\n\r\f\t\\\"") => "\"\\b\\n\\r\\f\\t\\\\\\\"\""
   (jsexpr->string "\37\40\177") => "\"\\u001f \\u007f\""
   (jsexpr->string "λ∀𝄞") => "\"λ∀𝄞\""
   (jsexpr->string "λ∀𝄞" #:encode 'all)
   => "\"\\u03bb\\u2200\\ud834\\udd1e\""
   ;; and that the same holds for keys
   (jsexpr->string (string->jsexpr "{\"\U0010FFFF\":\"\U0010FFFF\"}"))
   => "{\"\U0010FFFF\":\"\U0010FFFF\"}"
   (jsexpr->string #hash[(a . 1) (b . 2)]) => "{\"a\":1,\"b\":2}"
   (jsexpr->string (hash-copy #hash[(a . 1) (b . 2)])) => "{\"a\":1,\"b\":2}"
   (jsexpr->string (string->jsexpr "{\"\U0010FFFF\":\"\U0010FFFF\"}")
                   #:encode 'all)
   => "{\"\\udbff\\udfff\":\"\\udbff\\udfff\"}"

   ;; test json
   (json->string "\0\1\2\3") => "\"\\u0000\\u0001\\u0002\\u0003\""
   (json->string "\b\n\r\f\t\\\"") => "\"\\b\\n\\r\\f\\t\\\\\\\"\""
   (json->string "\37\40\177") => "\"\\u001f \\u007f\""
   (json->string "λ∀𝄞") => "\"λ∀𝄞\""
   (json->string "λ∀𝄞" #:encode 'all)
   => "\"\\u03bb\\u2200\\ud834\\udd1e\""
   ;; and that the same holds for keys
   (json->string (string->json #:mutable? #f "{\"\U0010FFFF\":\"\U0010FFFF\"}"))
   => "{\"\U0010FFFF\":\"\U0010FFFF\"}"
   (json->string #hash[(a . 1) (b . 2)]) => "{\"a\":1,\"b\":2}"
   (json->string (string->json #:mutable? #f "{\"\U0010FFFF\":\"\U0010FFFF\"}")
                 #:encode 'all)
   => "{\"\\udbff\\udfff\":\"\\udbff\\udfff\"}")

  ;; test inf
  (parameterize ([json-inf+ "+inf"]
                 [json-inf- "-inf"])
    (test (json->string JSON-inf+) => "\"+inf\""
          (jsexpr->string JSON-inf+) => "\"+inf\""
          (jsexpr->string json-inf+) => "\"+inf\""
          (jsexpr->string (json-inf+)) => "\"+inf\""

          (json->string JSON-inf-) => "\"-inf\""
          (jsexpr->string JSON-inf-) => "\"-inf\""
          (jsexpr->string json-inf-) => "\"-inf\""
          (jsexpr->string (json-inf-)) => "\"-inf\""))

  ;; test loop case
  (parameterize ([json-inf+ json-inf+]
                 [json-inf- json-inf-])
    (test (jsexpr->string JSON-inf+) =error> "jsexpr->string:"
          (jsexpr->string JSON-inf-) =error> "jsexpr->string:"
          (json->string JSON-inf+) =error> "json->string:"
          (json->string JSON-inf-) =error> "json->string:"))

  (parameterize ([json-inf+ JSON-inf+]
                 [json-inf- JSON-inf-])
    (test (jsexpr->string JSON-inf+) =error> "jsexpr->string:"
          (jsexpr->string JSON-inf-) =error> "jsexpr->string:"
          (json->string JSON-inf+) =error> "json->string:"
          (json->string JSON-inf-) =error> "json->string:")))

(define (parse-tests)
  (test
   ;; test jsexpr
   (string->jsexpr @T{  0   }) =>  0
   (string->jsexpr @T{  0x  }) =>  0 ; not clearly a good idea, but preserve old behavior
   (string->jsexpr @T{  1   }) =>  1
   (string->jsexpr @T{ -1   }) => -1 ; note: `+' is forbidden
   (string->jsexpr @T{ -12  }) => -12
   (string->jsexpr @T{ -123 }) => -123
   (string->jsexpr @T{  1.0 }) =>  1.0
   (string->jsexpr @T{  1.3 }) =>  1.3
   (string->jsexpr @T{  1.34}) =>  1.34
   (string->jsexpr @T{ -1.0 }) => -1.0
   (string->jsexpr @T{ -1.3 }) => -1.3
   (string->jsexpr @T{-10.34}) => -10.34
   (string->jsexpr @T{-10.34e3}) => -10340.0
   (string->jsexpr @T{-10.34e03}) => -10340.0
   (string->jsexpr @T{-10.34e+3}) => -10340.0
   (string->jsexpr @T{-10.34e+03}) => -10340.0
   (string->jsexpr @T{-10.34e+0x}) => -10.340 ; preserve old behavior
   (string->jsexpr @T{-10.34e-3}) => -1.034e-2
   (string->jsexpr @T{-10.34e-03}) => -1.034e-2
   (string->jsexpr @T{-10.34e+31}) => -1.034e32
   (string->jsexpr @T{ 1e9999999999999999    }) => +inf.0 ; breaks contract
   (string->jsexpr @T{ 1.0e9999999999999999  }) => +inf.0
   (string->jsexpr @T{-1e9999999999999999    }) => -inf.0
   (string->jsexpr @T{-1.0e9999999999999999  }) => -inf.0
   (string->jsexpr @T{ 1e-9999999999999999   }) =>  0.0
   (string->jsexpr @T{-1e-9999999999999999   }) => -0.0
   (string->jsexpr @T{ 0e9999999999999999    }) => 0.0
   (string->jsexpr @T{ 0e-9999999999999999   }) => 0.0
   (string->jsexpr @T{ 0.001e310 }) => 1.0e307
   (string->jsexpr @T{ 100000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000e-402 }) => 1e-292
   (string->jsexpr @T{ true  }) => #t
   (string->jsexpr @T{ false }) => #f
   (string->jsexpr @T{ null  }) => 'null
   (string->jsexpr @T{ ""    }) => ""
   (string->jsexpr @T{ "abc" }) => "abc"
   (string->jsexpr @T{ [] }) => '()
   (string->jsexpr @T{ [1,[2],3] }) => '(1 (2) 3)
   (string->jsexpr @T{ [ 1 , [ 2 ] , 3 ] }) => '(1 (2) 3)
   (string->jsexpr @T{ [true, false, null] }) => '(#t #f null)
   (immutable? (string->jsexpr @T{ {} }))
   (immutable? (string->jsexpr @T{ {"x":1} }))
   (immutable? (string->jsexpr @T{ {"x":1,"y":2} }))
   (string->jsexpr @T{ {} }) => '#hasheq()
   (string->jsexpr @T{ {"x":1} }) => '#hasheq([x . 1])
   (string->jsexpr @T{ {"x":1,"y":2} }) => '#hasheq([x . 1] [y . 2])
   (string->jsexpr @T{ [{"x": 1}, {"y": 2}] }) =>
   '(#hasheq([x . 1]) #hasheq([y . 2]))

   ;; string escapes
   (string->jsexpr @T{ " \b\n\r\f\t\\\"\/ " }) => " \b\n\r\f\t\\\"/ "
   (string->jsexpr @T{ "\uD834\uDD1E" }) => "\U1D11E"
   (string->jsexpr @T{ "\ud834\udd1e" }) => "\U1d11e"
   ;; INPUT PORT is optional
   (with-input-from-string "[]" read-jsexpr)
   => (parameterize ((json-null '())) (json-null))
   ;; EOF detection
   (for/list ([je (in-port read-jsexpr
                           (open-input-string
                            @T{ 1 [2,3] "four" }))])
     je)
   => '(1 (2 3) "four")
   (string->jsexpr "]") =error> "string->jsexpr:"
   (string->jsexpr "foo") =error> "string->jsexpr:"
   (string->jsexpr "") => eof
   (string->jsexpr " \t\r\n") => eof

   ;; Invalid UTF-8 input.
   (bytes->jsexpr #"\"\377\377\377\"") =error> exn:fail?

   ;; whitespace should be only the four allowed charactes
   (string->jsexpr (string-append
                    "{ \"x\" :"
                    (string #\u00A0)
                    " 123 }"))
   =error>
   #rx"whitespace that is not allowed"
   (string->jsexpr (string-append
                    "[ 1,"
                    (string #\u00A0)
                    " 2 ]"))
   =error>
   #rx"whitespace that is not allowed"

   ;; More string escapes:
   (string->jsexpr @T{ "hel\"lo" }) => "hel\"lo"
   (string->jsexpr @T{ "\\//\\\\//" }) => "\\//\\\\//"
   (string->jsexpr @T{ ["one", "t\\w\\o", 3] }) => '("one" "t\\w\\o" 3)
   (string->jsexpr @T{ "\u000A" }) => "\u000A"
   (string->jsexpr @T{ "/" }) => "/"
   (string->jsexpr @T{ "\/" }) => "/"
   ;; More error tests:
   (string->jsexpr @T{ -}) =error> #rx"string->jsexpr:.*-"
   (string->jsexpr @T{ -x}) =error> #rx"string->jsexpr:.*-x"
   (string->jsexpr @T{ 1.}) =error> #rx"string->jsexpr:.*1[.]"
   (string->jsexpr @T{ 1.x }) =error> #rx"string->jsexpr:.*1[.]x"
   (string->jsexpr @T{ -1.}) =error> #rx"string->jsexpr:.*-1[.]"
   (string->jsexpr @T{ -1.x }) =error> #rx"string->jsexpr:.*-1[.]x"
   (string->jsexpr @T{ -13e }) =error> #rx"string->jsexpr:.*-13e"
   (string->jsexpr @T{ -1.3e }) =error> #rx"string->jsexpr:.*-1[.]3e"
   (string->jsexpr @T{ -1.3ex}) =error> #rx"string->jsexpr:.*-1[.]3e"
   (string->jsexpr @T{ 10.3ex}) =error> #rx"string->jsexpr:.*10[.]3e"
   (string->jsexpr @T{ [00] }) =error> "string->jsexpr:"
   (string->jsexpr @T{ [1,2,,3] }) =error> "string->jsexpr:"
   (string->jsexpr @T{ [1,2,3,] }) =error> "string->jsexpr:"
   (string->jsexpr @T{ {42 : "bad-key!"} }) =error> "string->jsexpr:"
   (string->jsexpr @T{ {'no-colon' , ""} }) =error> "string->jsexpr:"
   (string->jsexpr @T{ {"x":1,,"y":2} }) =error> "string->jsexpr:"
   (string->jsexpr @T{ {x:1, y:2} }) =error> "string->jsexpr:"
   (string->jsexpr " {x:1, y:2] ") =error> "string->jsexpr:"
   (string->jsexpr " [\"x\",1, \"y\",2} ") =error> "string->jsexpr:"
   (string->jsexpr @T{ truelove }) =error> "string->jsexpr:"
   (string->jsexpr @T{ truebred }) =error> "string->jsexpr:"
   (string->jsexpr @T{ truea }) =error> "string->jsexpr:"
   (string->jsexpr @T{ falsehood }) =error> "string->jsexpr:"
   (string->jsexpr @T{ falsetto }) =error> "string->jsexpr:"
   (string->jsexpr @T{ nullity }) =error> "string->jsexpr:"
   (string->jsexpr @T{ nulliparous }) =error> "string->jsexpr:"


   ;; test cases to see if the trailing eof is consumed -- the rule
   ;; used to formulate these test cases is that an eof should be
   ;; consumed if and only if
   ;;  - eof is the entire thing in the stream (and thus it is returned), or
   ;;  - the eof triggered an error (ie the json object isn't complete)
   (let ([p (port-with-particulars (list #"1" eof #"a"))])
     (list (read-jsexpr p)
           (flush-data p)))
   => (list 1 (list eof 97))

   (let ([p (port-with-particulars (list eof #"a"))])
     (list (read-jsexpr p)
           (flush-data p)))
   =>
   (list eof (list 97))

   (let ([p (port-with-particulars (list eof eof #"a"))])
     (list (read-jsexpr p)
           (flush-data p)))
   =>
   (list eof (list eof 97))

   (let ([p (port-with-particulars (list eof eof #"a"))])
     (list (read-jsexpr p)
           (read-jsexpr p)
           (flush-data p)))
   =>
   (list eof eof (list 97))

   (let ([p (port-with-particulars (list #"\"1\"" eof eof #"a"))])
     (list (read-jsexpr p)
           (flush-data p)))
   => (list "1" (list eof eof 97))

   (let ([p (port-with-particulars (list #"\"1" eof eof #"a"))])
     (list (read-jsexpr/swallow-error p)
           (flush-data p)))
   => (list 'exn (list eof 97))

   (let ([p (port-with-particulars (list #"[1, 2]" eof eof #"a"))])
     (list (read-jsexpr p)
           (flush-data p)))
   => (list (list 1 2) (list eof eof 97))

   (let ([p (port-with-particulars (list #"[1, 2" eof eof #"a"))])
     (list (read-jsexpr/swallow-error p)
           (flush-data p)))
   => (list 'exn (list eof 97))

   (let ([p (port-with-particulars (list #"[1," eof eof #"a"))])
     (list (read-jsexpr/swallow-error p)
           (flush-data p)))
   => (list 'exn (list eof 97))

   (let ([p (port-with-particulars (list #"{ \"x\":  11 }" eof eof #"a"))])
     (list (read-jsexpr p)
           (flush-data p)))
   => (list (hasheq 'x 11) (list eof eof 97))

   (let ([p (port-with-particulars (list #"{ \"x\":  11 " eof eof #"a"))])
     (list (read-jsexpr/swallow-error p)
           (flush-data p)))
   => (list 'exn (list eof 97))

   (let ([p (port-with-particulars (list #"{ \"x\" " eof eof #"a"))])
     (list (read-jsexpr/swallow-error p)
           (flush-data p)))
   => (list 'exn (list eof 97))

   (let ([p (port-with-particulars (list #"{ \"x\" : " eof eof #"a"))])
     (list (read-jsexpr/swallow-error p)
           (flush-data p)))
   => (list 'exn (list eof 97))

   (let ([p (port-with-particulars (list #"{  " eof eof #"a"))])
     (list (read-jsexpr/swallow-error p)
           (flush-data p)))
   => (list 'exn (list eof 97))

   (let ([p (port-with-particulars (list #"{" eof eof #"a"))])
     (list (read-jsexpr/swallow-error p)
           (flush-data p)))
   => (list 'exn (list eof 97))

   (let ([p (port-with-particulars (list #"true" eof eof #"a"))])
     (list (read-jsexpr p)
           (flush-data p)))
   => (list #t (list eof eof 97))

   (let ([p (port-with-particulars (list #"false" eof eof #"a"))])
     (list (read-jsexpr p)
           (flush-data p)))
   => (list #f (list eof eof 97))

   (let ([p (port-with-particulars (list #"null" eof eof #"a"))])
     (list (read-jsexpr p)
           (flush-data p)))
   => (list 'null (list eof eof 97))

   ;; tests to make sure read-jsexpr doesn't hang when the
   ;; input is already enough to be sure we're doomed
   (read-jsexpr (port-with-particulars #"started"))
   =error> #rx"read-jsexpr: bad input starting #\"started\""

   (read-jsexpr (port-with-particulars #"try"))
   =error> #rx"read-jsexpr: bad input starting #\"try\""

   (read-jsexpr (port-with-particulars #"falz"))
   =error> #rx"read-jsexpr: bad input starting #\"falz\""

   (read-jsexpr (port-with-particulars #"noll"))
   =error> #rx"read-jsexpr: bad input starting #\"noll\""


   ;; test json
   (string->json #:mutable? #f @T{  0   }) =>  0
   (string->json #:mutable? #f @T{  0x  }) =>  0 ; not clearly a good idea, but preserve old behavior
   (string->json #:mutable? #f @T{  1   }) =>  1
   (string->json #:mutable? #f @T{ -1   }) => -1 ; note: `+' is forbidden
   (string->json #:mutable? #f @T{ -12  }) => -12
   (string->json #:mutable? #f @T{ -123 }) => -123
   (string->json #:mutable? #f @T{  1.0 }) =>  1.0
   (string->json #:mutable? #f @T{  1.3 }) =>  1.3
   (string->json #:mutable? #f @T{  1.34}) =>  1.34
   (string->json #:mutable? #f @T{ -1.0 }) => -1.0
   (string->json #:mutable? #f @T{ -1.3 }) => -1.3
   (string->json #:mutable? #f @T{-10.34}) => -10.34
   (string->json #:mutable? #f @T{-10.34e3}) => -10340.0
   (string->json #:mutable? #f @T{-10.34e03}) => -10340.0
   (string->json #:mutable? #f @T{-10.34e+3}) => -10340.0
   (string->json #:mutable? #f @T{-10.34e+03}) => -10340.0
   (string->json #:mutable? #f @T{-10.34e+0x}) => -10.340 ; preserve old behavior
   (string->json #:mutable? #f @T{-10.34e-3}) => -1.034e-2
   (string->json #:mutable? #f @T{-10.34e-03}) => -1.034e-2
   (string->json #:mutable? #f @T{-10.34e+31}) => -1.034e32
   (string->json #:mutable? #f @T{ 1e9999999999999999    }) => JSON-inf+
   (string->json #:mutable? #f @T{ 1.0e9999999999999999  }) => JSON-inf+
   (string->json #:mutable? #f @T{-1e9999999999999999    }) => JSON-inf-
   (string->json #:mutable? #f @T{-1.0e9999999999999999  }) => JSON-inf-
   (string->json #:mutable? #f @T{ 1e-9999999999999999   }) =>  0.0
   (string->json #:mutable? #f @T{-1e-9999999999999999   }) => -0.0
   (string->json #:mutable? #f @T{ 0e9999999999999999    }) => 0.0
   (string->json #:mutable? #f @T{ 0e-9999999999999999   }) => 0.0
   (string->json #:mutable? #f @T{ 0.001e310 }) => 1.0e307
   (string->json #:mutable? #f @T{ 100000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000e-402 }) => 1e-292
   (string->json #:mutable? #f @T{ true  }) => #t
   (string->json #:mutable? #f @T{ false }) => #f
   (string->json #:mutable? #f @T{ null  }) => JSON-null
   (string->json #:mutable? #f @T{ ""    }) => ""
   (string->json #:mutable? #f @T{ "abc" }) => "abc"
   (string->json #:mutable? #f @T{ [] }) => '()
   (string->json #:mutable? #f @T{ [1,[2],3] }) => '(1 (2) 3)
   (string->json #:mutable? #f @T{ [ 1 , [ 2 ] , 3 ] }) => '(1 (2) 3)
   (string->json #:mutable? #f @T{ [true, false, null] }) => `(#t #f ,JSON-null)
   (immutable? (string->json #:mutable? #f @T{ {} }))
   (immutable? (string->json #:mutable? #f @T{ {"x":1} }))
   (immutable? (string->json #:mutable? #f @T{ {"x":1,"y":2} }))
   (string->json #:mutable? #f @T{ {} }) => '#hasheq()
   (string->json #:mutable? #f @T{ {"x":1} }) => '#hasheq([x . 1])
   (string->json #:mutable? #f @T{ {"x":1,"y":2} }) => '#hasheq([x . 1] [y . 2])
   (string->json #:mutable? #f @T{ [{"x": 1}, {"y": 2}] }) =>
   '(#hasheq([x . 1]) #hasheq([y . 2]))

   ;; string escapes
   (string->json #:mutable? #f @T{ " \b\n\r\f\t\\\"\/ " }) => " \b\n\r\f\t\\\"/ "
   (string->json #:mutable? #f @T{ "\uD834\uDD1E" }) => "\U1D11E"
   (string->json #:mutable? #f @T{ "\ud834\udd1e" }) => "\U1d11e"
   ;; INPUT PORT is optional
   (with-input-from-string "[]" (λ () (read-JSON #:mutable? #f)))
   => (parameterize ((json-null '())) (json-null))
   ;; EOF detection
   (for/list ([je (in-port (λ (in) (read-JSON #:mutable? #f in))
                           (open-input-string
                            @T{ 1 [2,3] "four" }))])
     je)
   => '(1 (2 3) "four")
   (string->json #:mutable? #f "]") =error> "string->json:"
   (string->json #:mutable? #f "foo") =error> "string->json:"
   (string->json #:mutable? #f "") => eof
   (string->json #:mutable? #f " \t\r\n") => eof

   ;; Invalid UTF-8 input.
   (bytes->json #:mutable? #f #"\"\377\377\377\"") =error> exn:fail?

   ;; whitespace should be only the four allowed charactes
   (string->json #:mutable? #f (string-append
                  "{ \"x\" :"
                  (string #\u00A0)
                  " 123 }"))
   =error>
   #rx"whitespace that is not allowed"
   (string->json #:mutable? #f (string-append
                  "[ 1,"
                  (string #\u00A0)
                  " 2 ]"))
   =error>
   #rx"whitespace that is not allowed"

   ;; More string escapes:
   (string->json #:mutable? #f @T{ "hel\"lo" }) => "hel\"lo"
   (string->json #:mutable? #f @T{ "\\//\\\\//" }) => "\\//\\\\//"
   (string->json #:mutable? #f @T{ ["one", "t\\w\\o", 3] }) => '("one" "t\\w\\o" 3)
   (string->json #:mutable? #f @T{ "\u000A" }) => "\u000A"
   (string->json #:mutable? #f @T{ "/" }) => "/"
   (string->json #:mutable? #f @T{ "\/" }) => "/"
   ;; More error tests:
   (string->json #:mutable? #f @T{ -}) =error> #rx"string->json:.*-"
   (string->json #:mutable? #f @T{ -x}) =error> #rx"string->json:.*-x"
   (string->json #:mutable? #f @T{ 1.}) =error> #rx"string->json:.*1[.]"
   (string->json #:mutable? #f @T{ 1.x }) =error> #rx"string->json:.*1[.]x"
   (string->json #:mutable? #f @T{ -1.}) =error> #rx"string->json:.*-1[.]"
   (string->json #:mutable? #f @T{ -1.x }) =error> #rx"string->json:.*-1[.]x"
   (string->json #:mutable? #f @T{ -13e }) =error> #rx"string->json:.*-13e"
   (string->json #:mutable? #f @T{ -1.3e }) =error> #rx"string->json:.*-1[.]3e"
   (string->json #:mutable? #f @T{ -1.3ex}) =error> #rx"string->json:.*-1[.]3e"
   (string->json #:mutable? #f @T{ 10.3ex}) =error> #rx"string->json:.*10[.]3e"
   (string->json #:mutable? #f @T{ [00] }) =error> "string->json:"
   (string->json #:mutable? #f @T{ [1,2,,3] }) =error> "string->json:"
   (string->json #:mutable? #f @T{ [1,2,3,] }) =error> "string->json:"
   (string->json #:mutable? #f @T{ {42 : "bad-key!"} }) =error> "string->json:"
   (string->json #:mutable? #f @T{ {'no-colon' , ""} }) =error> "string->json:"
   (string->json #:mutable? #f @T{ {"x":1,,"y":2} }) =error> "string->json:"
   (string->json #:mutable? #f @T{ {x:1, y:2} }) =error> "string->json:"
   (string->json #:mutable? #f " {x:1, y:2] ") =error> "string->json:"
   (string->json #:mutable? #f " [\"x\",1, \"y\",2} ") =error> "string->json:"
   (string->json #:mutable? #f @T{ truelove }) =error> "string->json:"
   (string->json #:mutable? #f @T{ truebred }) =error> "string->json:"
   (string->json #:mutable? #f @T{ truea }) =error> "string->json:"
   (string->json #:mutable? #f @T{ falsehood }) =error> "string->json:"
   (string->json #:mutable? #f @T{ falsetto }) =error> "string->json:"
   (string->json #:mutable? #f @T{ nullity }) =error> "string->json:"
   (string->json #:mutable? #f @T{ nulliparous }) =error> "string->json:"


   ;; test cases to see if the trailing eof is consumed -- the rule
   ;; used to formulate these test cases is that an eof should be
   ;; consumed if and only if
   ;;  - eof is the entire thing in the stream (and thus it is returned), or
   ;;  - the eof triggered an error (ie the json object isn't complete)
   (let ([p (port-with-particulars (list #"1" eof #"a"))])
     (list (read-JSON p #:mutable? #f)
           (flush-data p)))
   => (list 1 (list eof 97))

   (let ([p (port-with-particulars (list eof #"a"))])
     (list (read-JSON p #:mutable? #f)
           (flush-data p)))
   =>
   (list eof (list 97))

   (let ([p (port-with-particulars (list eof eof #"a"))])
     (list (read-JSON p #:mutable? #f)
           (flush-data p)))
   =>
   (list eof (list eof 97))

   (let ([p (port-with-particulars (list eof eof #"a"))])
     (list (read-JSON p #:mutable? #f)
           (read-JSON p #:mutable? #f)
           (flush-data p)))
   =>
   (list eof eof (list 97))

   (let ([p (port-with-particulars (list #"\"1\"" eof eof #"a"))])
     (list (read-JSON p #:mutable? #f)
           (flush-data p)))
   => (list "1" (list eof eof 97))

   (let ([p (port-with-particulars (list #"\"1" eof eof #"a"))])
     (list (read-JSON/swallow-error p #:mutable? #f)
           (flush-data p)))
   => (list 'exn (list eof 97))

   (let ([p (port-with-particulars (list #"[1, 2]" eof eof #"a"))])
     (list (read-JSON p #:mutable? #f)
           (flush-data p)))
   => (list (list 1 2) (list eof eof 97))

   (let ([p (port-with-particulars (list #"[1, 2" eof eof #"a"))])
     (list (read-JSON/swallow-error p #:mutable? #f)
           (flush-data p)))
   => (list 'exn (list eof 97))

   (let ([p (port-with-particulars (list #"[1," eof eof #"a"))])
     (list (read-JSON/swallow-error p #:mutable? #f)
           (flush-data p)))
   => (list 'exn (list eof 97))

   (let ([p (port-with-particulars (list #"{ \"x\":  11 }" eof eof #"a"))])
     (list (read-JSON p #:mutable? #f)
           (flush-data p)))
   => (list (hasheq 'x 11) (list eof eof 97))

   (let ([p (port-with-particulars (list #"{ \"x\":  11 " eof eof #"a"))])
     (list (read-JSON/swallow-error p #:mutable? #f)
           (flush-data p)))
   => (list 'exn (list eof 97))

   (let ([p (port-with-particulars (list #"{ \"x\" " eof eof #"a"))])
     (list (read-JSON/swallow-error p #:mutable? #f)
           (flush-data p)))
   => (list 'exn (list eof 97))

   (let ([p (port-with-particulars (list #"{ \"x\" : " eof eof #"a"))])
     (list (read-JSON/swallow-error p #:mutable? #f)
           (flush-data p)))
   => (list 'exn (list eof 97))

   (let ([p (port-with-particulars (list #"{  " eof eof #"a"))])
     (list (read-JSON/swallow-error p #:mutable? #f)
           (flush-data p)))
   => (list 'exn (list eof 97))

   (let ([p (port-with-particulars (list #"{" eof eof #"a"))])
     (list (read-JSON/swallow-error p #:mutable? #f)
           (flush-data p)))
   => (list 'exn (list eof 97))

   (let ([p (port-with-particulars (list #"true" eof eof #"a"))])
     (list (read-JSON p #:mutable? #f)
           (flush-data p)))
   => (list #t (list eof eof 97))

   (let ([p (port-with-particulars (list #"false" eof eof #"a"))])
     (list (read-JSON p #:mutable? #f)
           (flush-data p)))
   => (list #f (list eof eof 97))

   (let ([p (port-with-particulars (list #"null" eof eof #"a"))])
     (list (read-JSON p #:mutable? #f)
           (flush-data p)))
   => (list JSON-null (list eof eof 97))

   ;; tests to make sure read-JSON doesn't hang when the
   ;; input is already enough to be sure we're doomed
   (read-JSON #:mutable? #f (port-with-particulars #"started"))
   =error> #rx"read-JSON: bad input starting #\"started\""

   (read-JSON #:mutable? #f (port-with-particulars #"try"))
   =error> #rx"read-JSON: bad input starting #\"try\""

   (read-JSON #:mutable? #f (port-with-particulars #"falz"))
   =error> #rx"read-JSON: bad input starting #\"falz\""

   (read-JSON #:mutable? #f (port-with-particulars #"noll"))
   =error> #rx"read-JSON: bad input starting #\"noll\"")

  ;; test jsexpr-mhash?
  (parameterize ([jsexpr-mhash? #t])
    (test
     (not (immutable? (string->jsexpr @T{ {} })))
     (not (immutable? (string->jsexpr @T{ {"x":1} })))
     (not (immutable? (string->jsexpr @T{ {"x":1,"y":2} }))))))

(define (convert-tests)
  (for ([x (list 0 1 -1 12345 0.0 1.0 #t #f (λ (n) n) "" "abc" "abc\n\\"
                 '() '(1 2 3) (λ (n) `(1 "2" (3) #t #f ,n)) '((((()))))
                 '#hasheq()
                 '#hasheq([x . 1])
                 '#hasheq([x . 1] [y . 2])
                 '#hasheq([|x\y| . 1] [y . 2])
                 ;; string escapes
                 "λ" "\U1D11E" ; goes as a plain character in normal encoding
                 "\0" "\1" "\2" "\3" "\37" "\177" ; encoded as json \u escapes
                 "\b" "\n" "\r" "\f" "\t"         ; same escapes in both
                 "\a" "\v" "\e"                   ; does not use racket escapes
                 )])
    (define (N x null) (if (procedure? x) (x null) x))
    (test
     ;; default
     (json->jsexpr (jsexpr->json #:mutable? #f (N x 'null)))
     => (N x 'null)
     ;; different null
     (json->jsexpr (jsexpr->json #:mutable? #f (N x #\null) #:null #\null) #:null #\null)
     => (N x #\null)
     ;; JSON-null
     (jsexpr->json #:mutable? #f (json->jsexpr (N x JSON-null)))
     => (N x JSON-null))))

(module port-with-particulars racket/base
  (require tests/eli-tester racket/contract)
  (provide
   (contract-out
    [port-with-particulars
     ;; produces a port that produces the data in `data`
     ;; taking special care to return exactly the given `eof`s.
     ;; When we run out of data, the port just blocks forever.
     ;; NB: this port doesn't work if accessed from multiple threads
     (-> (flat-rec-contract data (or/c (cons/c data data) '() bytes? eof-object?))
         (and/c input-port?
                port-with-particulars?))]
    [flush-data
     ;; get the remaining data in the port in a convenient form for test cases
     (-> port-with-particulars? (listof (or/c byte? eof-object?)))]))

  (struct port-with-particulars (port get-remaining-data)
    #:property prop:input-port 0
    #:constructor-name make-port-with-particulars
    #:name make-port-with-particulars)

  (define (flush-data p)
    ((port-with-particulars-get-remaining-data p)))

  (define (port-with-particulars data)

    (define this data)
    (define next #f)

    (define (get-next #:peek? [peek? #f])
      (let loop ()
        (cond
          [(and (not this) (not next))
           #f]
          [(pair? this)
           (cond
             [(or (eof-object? (car this))
                  (byte? (car this)))
              (define ans (car this))
              (set! this (cdr this))
              ans]
             [(or (not (car this)) (null? (car this)))
              (set! this (cdr this))
              (loop)]
             [else
              (set! next (cons (cdr this) next))
              (set! this (car this))
              (loop)])]
          [(or (eof-object? this) (byte? this))
           (begin0 this
             (set! this next)
             (set! next #f))]
          [(or (null? this) (not this))
           (set! this next)
           (set! next #f)
           (loop)]
          [(bytes? this)
           (set! this (bytes->list this))
           (loop)]
          [else (error 'next "internal error ~s" this)])))

    (define (read-in bts)
      (define byte (get-next))
      (cond
        [(not byte) never-evt]
        [(eof-object? byte) byte]
        [else
         (bytes-set! bts 0 byte)
         1]))

    (define (peek bts i evt)
      (define hit-eof? #f)
      (define hit-end-of-data? #f)
      (define skipped
        (let loop ([i i])
          (cond
            [(zero? i) '()]
            [else
             (define n (get-next))
             (cond
               [(not n)
                (set! hit-end-of-data? #t)
                '()]
               [(eof-object? n)
                (set! hit-eof? #t)
                (cons eof '())]
               [else (cons n (loop (- i 1)))])])))
      (cond
        [hit-end-of-data?
         (set! next (cons this next))
         (set! this skipped)
         never-evt]
        [hit-eof?
         (set! next (cons this next))
         (set! this skipped)
         eof]
        [else
         (let loop ([bytes-peeked 0]
                    [to-restore '()])
           (cond
             [(< bytes-peeked (bytes-length bts))
              (define peeked (get-next))
              (cond
                [(not peeked)
                 ;; skipped, (reverse to-restore), this, next is the
                 ;; right order to restore, but since we ran out of data,
                 ;; both this and next are #f right now.
                 (set! this (list skipped (reverse to-restore)))
                 (set! next #f)
                 (if (= bytes-peeked 0)
                     never-evt
                     bytes-peeked)]
                [(eof-object? peeked)
                 ;; skipped, (reverse to-restore), peeked, this, next
                 ;; is the right order, just use `this` to put them back
                 (set! this (list skipped (reverse to-restore) peeked this))
                 (if (= bytes-peeked 0)
                     eof
                     bytes-peeked)]
                [else
                 (bytes-set! bts bytes-peeked peeked)
                 (loop (+ bytes-peeked 1) (cons peeked to-restore))])]
             [else
              ;; skipped, (reverse to-restore), this, next
              ;; is the right order, just use `this` to put them back
              (set! this (list skipped (reverse to-restore) this))
              bytes-peeked]))]))

    (define (get-remaining-data)
      (let loop ()
        (define next (get-next))
        (cond
          [(not next) '()]
          [else (cons next (loop))])))

    (make-port-with-particulars
     (make-input-port
      (format "port-with-particulars: ~s" data)
      read-in
      peek
      void)
     get-remaining-data)))


(require 'port-with-particulars)
(define (port-with-particulars-tests)
  (test (port->bytes (port-with-particulars (cons #"abc" eof))) => #"abc"
        (port->bytes (port-with-particulars (cons (cons #"a" #"bc") eof))) => #"abc"
        (port->bytes (port-with-particulars (cons (list #"a" #"" #"b" #"")
                                                  (list #"c" #"" eof))))
        => #"abc"
        (port->bytes (port-with-particulars (cons #"a" (cons #"bc" eof)))) => #"abc"
        (let ([p (port-with-particulars (list #"abc" eof #"def" eof))])
          (list (port->bytes p)
                (port->bytes p)))
        => (list #"abc" #"def")
        (let ([p (port-with-particulars (cons #"a" (cons #"bc" eof)))])
          (read-byte p)
          (flush-data p))
        =>
        (cons 98 (cons 99 (cons eof '())))

        (let ([p (port-with-particulars (list eof eof))])
          (list (peek-char p)
                (flush-data p)))
        =>
        (list eof (list eof eof))

        (let ([p (port-with-particulars (list #"abc" eof #"def"))])
          (define p2 (peeking-input-port p))
          (list (read-char p2)
                (read-char p2)
                (read-char p2)
                (read-char p2)
                (read-char p2)
                (flush-data p)))
        =>
        (list #\a #\b #\c eof eof
              (list 97 98 99 eof 100 101 102))

        (let ([p (port-with-particulars (list (cons #"a" #"b") (cons #"c" eof) #"def"))])
          (define p2 (peeking-input-port p))
          (list (read-char p2)
                (read-char p2)
                (read-char p2)
                (read-char p2)
                (read-char p2)
                (flush-data p)))
        =>
        (list #\a #\b #\c eof eof
              (list 97 98 99 eof 100 101 102))

        (let ([p (port-with-particulars (list (cons #"a" #"b") (cons #"c" eof) #"def"))])
          (define b (make-bytes 5))
          (peek-bytes-avail! b 0 #f p)
          b)
        =>
        #"abc\0\0"))

(define (read-JSON/swallow-error p #:mutable? mutable?)
  (with-handlers ([(λ (x) (and (exn:fail:read? x)
                                (regexp-match #rx"^[^\n]*read-JSON:" (exn-message x))))
                   (λ (x) 'exn)])
    (read-JSON #:mutable? mutable? p)
    (error 'read-JSON/swallow-error "did not raise an error")))

(define (read-jsexpr/swallow-error p)
  (with-handlers ([(λ (x) (and (exn:fail:read? x)
                                (regexp-match #rx"^[^\n]*read-jsexpr:" (exn-message x))))
                   (λ (x) 'exn)])
    (read-jsexpr p)
    (error 'read-jsexpr/swallow-error "did not raise an error")))

(test do (port-with-particulars-tests)
      do (pred-tests)
      do (print-tests)
      do (parse-tests)
      do (convert-tests)
      )
