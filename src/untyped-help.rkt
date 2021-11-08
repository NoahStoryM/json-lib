#lang typed/racket/base/no-check

(require compatibility/mlist)

(provide mlist?
         mmap
         mmap->list
         map->mlist
         andmmap
         ormmap)


(: mmap       (All (A B) [-> [-> A B] (MListof A) (MListof B)]))
(: mmap->list (All (A B) [-> [-> A B] (MListof A) (Listof  B)]))
(: map->mlist (All (A B) [-> [-> A B] (Listof  A) (MListof B)]))
(define-values (mmap mmap->list map->mlist)
  (let ()
    (define ((make s-car s-cdr t-cons) f l)
      (let loop ([l l] [res '()])
        (if (null? l) res (loop (s-cdr l) (t-cons (f (s-car l)) res)))))

    (values (make mcar mcdr mcons)
            (make mcar mcdr  cons)
            (make  car  cdr mcons))))

(: andmmap (All (A B) [-> [-> A B] (MListof A) (U Boolean B)]))
(define (andmmap f l)
  (let loop ([l l])
    (if (null? l) #t (and (f (mcar l)) (loop (mcdr l))))))

(: ormmap  (All (A B) [-> [-> A B] (MListof A) (U False B)]))
(define (ormmap  f l)
  (let loop ([l l])
    (if (null? l) #f (or (f (mcar l)) (loop (mcdr l))))))
