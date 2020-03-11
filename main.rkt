#lang racket

(require "lib.rkt")
(provide (all-from-out "lib.rkt"))

(module reader racket/base
  (require "lib.rkt")
  (provide (rename-out [r read] [rs read-syntax]))
  (define-values (r rs) (make-tesurell-lang)))
