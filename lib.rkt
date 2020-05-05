#lang racket/base

(provide embed
         module/port
         reformat-doc)
(require syntax/modread
         syntax/strip-context
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

(define (inline-module i s . strs)
  (module/port i s (open-input-string (apply string-append strs))))

(define (embed id sym . strs)
  (if (string? sym)
      (apply inline-module id #f (cons sym strs))
      (apply inline-module id sym strs)))
