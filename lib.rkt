#lang racket/base

(provide make-tesurell-lang
         run-markup
         embed
         module/port
         reformat-doc)
(require scribble/reader
         syntax/modread
         racket/list
         racket/string)

(define (aggregate-strings strings)
  (filter-map (λ (x) (and (not (equal? "" x))
                          (regexp-replace* #px"\\s\\s*" x " ")))
              (regexp-split #px"\n\n+"
                            (string-trim (string-join strings "")))))

(define (reformat-doc doc)
  (if (eq? doc '()) doc
      (let ([next (car doc)])
        (if (void? next)
            (reformat-doc (cdr doc))
            (let-values ([(strings remaining) (splitf-at doc string?)])
              (if (eq? strings '())
                  (cons next (reformat-doc (cdr doc)))
                  (append (aggregate-strings strings)
                          (reformat-doc remaining))))))))

(define (make-tesurell-lang [preval '#f])
  (define (markup-read-syntax src in)
    (with-syntax ([code (read-syntax-inside (object-name in) in)])
      #`(module content racket/base
          (require tesurell/lib)
          (provide doc)
          (define doc (run-markup '#,preval #'code))
          (module+ main
            (writeln doc)))))

  (define (markup-read in)
    (syntax->datum (markup-read-syntax #f in)))

  (values markup-read
          markup-read-syntax))

(define (eval-iter exprs acc)
  (if (eq? exprs '())
      (reverse acc)
      (eval-iter (cdr exprs)
                 (cons (eval (syntax->datum (car exprs))) acc))))

(define (run-markup preval stx)
  (define ns (make-base-namespace))
  (parameterize ([current-namespace ns]
                 [current-module-declare-name #f])
    (namespace-set-variable-value! 'embed embed #t ns #t)
    (eval preval)
    (define raw (eval-iter (syntax-e (namespace-syntax-introduce stx)) '()))
    ((namespace-variable-value 'make-doc #t (λ _ reformat-doc)) raw)))

(define (module/port id sym in [ns (current-namespace)])
  (define (read-thunk) (read-syntax (object-name in) in))
  (define user-stx (with-module-reading-parameterization read-thunk))
  (define checked (check-module-form user-stx '_ "inline module"))
  (define datum (syntax->datum checked))
  (define renamed (apply list (car datum) id (cddr datum)))
  (parameterize ([current-namespace ns])
    (eval renamed)
    (if (symbol? sym)
        (eval `(begin (require (only-in ',id ,sym)) ,sym))
        (void))))

(define (embed id sym . strs)
  (define (inline-module i s . strs)
    (module/port i s (open-input-string (apply string-append strs))))
  (if (string? sym)
      (apply inline-module id #f (cons sym strs))
      (apply inline-module id sym strs)))
