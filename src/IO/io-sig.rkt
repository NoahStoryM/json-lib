#lang racket/base

(require racket/unit
         "../types.rkt")

(provide io^)


(define-signature io^
  (write-JSON*
   read-JSON*))
