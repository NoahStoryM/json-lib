#lang racket/base

(provide identity
         mlist?
         mreverse
         list->mlist
         mlist->list
         mmap
         mmap->list
         map->mlist
         andmmap
         ormmap)

(define identity (λ (arg) arg))

(define (mlist? l)
  (cond
    [(null? l) #t]
    [(mpair? l)
     (let loop ([turtle l] [hare (mcdr l)])
       (cond
         [(null? hare) #t]
         [(eq? hare turtle) #f]
         [(mpair? hare)
          (let ([hare (mcdr hare)])
            (cond
              [(null? hare) #t]
              [(eq? hare turtle) #f]
              [(mpair? hare)
               (loop (mcdr turtle) (mcdr hare))]
              [else #f]))]
         [else #f]))]
    [else #f]))

(define-values (mreverse list->mlist mlist->list)
  (let ()
    (define ((make s-car s-cdr t-cons return) l)
      (let loop ([l l] [res '()])
        (if (null? l)
            ((return) res)
            (loop (s-cdr l) (t-cons (s-car l) res)))))

    (values (make mcar mcdr mcons (λ () identity))
            (make  car  cdr mcons (λ () mreverse))
            (make mcar mcdr  cons (λ () reverse)))))

(define-values (mmap mmap->list map->mlist)
  (let ()
    (define ((make s-car s-cdr t-cons return) f l)
      (let loop ([l l] [res '()])
        (if (null? l) (return res) (loop (s-cdr l) (t-cons (f (s-car l)) res)))))

    (values (make mcar mcdr mcons mreverse)
            (make mcar mcdr  cons  reverse)
            (make  car  cdr mcons mreverse))))

(define (andmmap f l)
  (let loop ([l l])
    (if (null? l) #t (and (f (mcar l)) (loop (mcdr l))))))

(define (ormmap  f l)
  (let loop ([l l])
    (if (null? l) #f (or (f (mcar l)) (loop (mcdr l))))))
