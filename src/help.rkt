#lang typed/racket/base

(provide mmap
         ;; andmmap
         ;; ormmap
         mreverse
         list->mlist
         mlist->list)


(: mmap (All (A B) [-> [-> A B] (MListof A) (MListof B)]))
(define (mmap f l)
  (let loop ([l l] [res : (MListof B) '()])
    (if (null? l) res (loop (mcdr l) (mcons (f (mcar l)) res)))))

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
(define (list->mlist l)
  (let loop ([l l] [res : (MListof A) '()])
    (if (null? l) (mreverse res) (loop (cdr l) (mcons (car l) res)))))

(: mlist->list (All (A) [-> (MListof A) (Listof A)]))
(define (mlist->list l)
  (let loop ([l l] [res : (Listof A) '()])
    (if (null? l) (reverse  res) (loop (mcdr l) (cons (mcar l) res)))))
