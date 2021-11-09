#lang typed/racket/base

(provide mmap
         mmap->list
         map->mlist
         ;; andmmap
         ;; ormmap
         mreverse
         list->mlist
         mlist->list)


(: id (All (A) [-> A A]))
(define id (λ (arg) arg))


(: mmap (All (A B) [-> [-> A B] (MListof A) (MListof B)]))
(define (mmap f l)
  (let loop ([l l] [res : (MListof B) '()])
    (if (null? l) (mreverse res) (loop (mcdr l) (mcons (f (mcar l)) res)))))

(: mmap->list (All (A B) [-> [-> A B] (MListof A) (Listof B)]))
(define (mmap->list f l)
  (let loop ([l l] [res : (Listof B) '()])
    (if (null? l) (reverse  res) (loop (mcdr l) (cons (f (mcar l)) res)))))

(: map->mlist (All (A B) [-> [-> A B] (Listof A) (MListof B)]))
(define (map->mlist f l)
  (let loop ([l l] [res : (MListof B) '()])
    (if (null? l) (mreverse res) (loop (cdr l) (mcons (f (car l)) res)))))

;; (: andmmap (All (A B) [-> [-> A B] (MListof A) (U True  B)]))
;; (define (andmmap f l)
;;   (let loop ([l l])
;;     (if (null? l) #t (and (f (mcar l)) (loop (mcdr l))))))

;; (: ormmap  (All (A B) [-> [-> A B] (MListof A) (U False B)]))
;; (define (ormmap  f l)
;;   (let loop ([l l])
;;     (if (null? l) #f (or (f (mcar l)) (loop (mcdr l))))))

(: mreverse (All (A) [-> (MListof A) (MListof A)]))
(define (mreverse l)
  (let loop ([l l] [res : (MListof A) '()])
    (if (null? l) res (loop (mcdr l) (mcons (mcar l) res)))))

(: list->mlist (All (A) [-> (Listof A) (MListof A)]))
(define (list->mlist l) (map->mlist (inst id A) l))

(: mlist->list (All (A) [-> (MListof A) (Listof A)]))
(define (mlist->list l) (mmap->list (inst id A) l))
