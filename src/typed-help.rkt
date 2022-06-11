#lang typed/racket/base

(require typed/racket/unsafe)

(unsafe-require/typed/provide compatibility/mlist
  [mlist (All (A) [-> A * (MListof A)])]
  [mmap (All (A B) [-> [-> A B] (MListof A) (MListof B)])]
  [mlength [-> (U Null MPairTop) Index]]
  [mreverse (All (A) [-> (MListof A) (MListof A)])]
  [list->mlist (All (A) [-> (Listof A) (MListof A)])]
  [mlist->list (All (A) [-> (MListof A) (Listof A)])])

(provide mmap->list map->mlist #;andmmap #;ormmap)

(: mmap->list (All (A B) [-> [-> A B] (MListof A) (Listof B)]))
(define (mmap->list f l)
  (let loop ([l l] [res : (Listof B) '()])
    (if (null? l) (reverse  res) (loop (mcdr l) (cons (f (mcar l)) res)))))

(: map->mlist (All (A B) [-> [-> A B] (Listof A) (MListof B)]))
(define (map->mlist f l)
  (let loop ([l l] [res : (MListof B) '()])
    (if (null? l) (mreverse res) (loop (cdr l) (mcons (f (car l)) res)))))

#;(: andmmap (All (A B) [-> [-> A B] (MListof A) (U True  B)]))
#;(define (andmmap f l)
    (let loop ([l l])
      (if (null? l) #t (and (f (mcar l)) (loop (mcdr l))))))

#;(: ormmap  (All (A B) [-> [-> A B] (MListof A) (U False B)]))
#;(define (ormmap  f l)
    (let loop ([l l])
      (if (null? l) #f (or (f (mcar l)) (loop (mcdr l))))))

