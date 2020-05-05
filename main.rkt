#lang racket

(require "lib.rkt")
(provide (all-from-out "lib.rkt"))

(module reader racket/base
  (provide (rename-out [read+ read] [read-syntax+ read-syntax]))
  (require "lib.rkt"
           racket/syntax
           syntax/strip-context
           scribble/reader)

  (define (read-syntax+ src in)
    (with-syntax ([code (read-syntax-inside (object-name in) in)]
                  [src-string (and src (path->string src))])
    #`(module content racket/base
        (provide doc)
        (require tesurell/lib)
        (define-namespace-anchor $anchor)
        (define $src (and src-string (string->path src-string)))
        (define $module-namespace (namespace-anchor->namespace $anchor))
        (define $raw
          (for/list ([expr (in-list '#,(namespace-syntax-introduce (strip-context #'code)))])
            (eval expr $module-namespace)))
        (define post
          (namespace-variable-value 'make-doc #t (λ () reformat-doc)
                                    $module-namespace))
        (define doc (post $raw))
        (module+ main
          (writeln doc)))))

  (define (read+ in)
    (syntax->datum (read-syntax+ #f in))))
