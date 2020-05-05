#lang info
(define collection "tesurell")
(define deps '("base" "compatibility-lib" "at-exp-lib"))
(define build-deps '("scribble-lib" "racket-doc" "rackunit-lib" "at-exp-lib"))
(define scribblings '(("scribblings/tesurell.scrbl" ())))
(define pkg-desc "Convenient means to switch #langs within one file.")
(define version "1.0")
(define pkg-authors '(sage))
