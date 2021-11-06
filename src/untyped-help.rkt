#lang typed/racket/base/no-check

(require compatibility/mlist)

(provide mlist?
         andmmap
         ormmap)

(: andmmap (All (A B) [-> [-> A B] (MListof A) (U True  B)]))
(define (andmmap f l)
  (let loop ([l l])
    (if (null? l) #t (and (f (mcar l)) (loop (mcdr l))))))

(: ormmap  (All (A B) [-> [-> A B] (MListof A) (U False B)]))
(define (ormmap  f l)
  (let loop ([l l])
    (if (null? l) #f (or (f (mcar l)) (loop (mcdr l))))))
