#lang scribble/manual
@require[@for-label[tesurell
                    racket/base
                    racket/contract
                    racket/list
                    racket/string
                    [except-in scribble/reader read read-syntax]]]

@title{Tesurell: A Self-hosting Melting Pot of Languages}
@author{Sage Gerard}

@defmodule[tesurell #:lang]

Tesurell is a markup language that supports inline use of other
@litchar{#lang}s, including itself. When used as a module, Tesurell
helps you use @litchar{#lang}s via input ports, and helps you
define other languages that support inline @litchar{#lang}s.

@section{Motivation}
When I write Racket programs using different languages I end up with a
bunch of files. That makes sense when those files represent modular
components in a sufficiently large system.  Thing is, I don't always
want to bounce between files to express one composite idea.

Other libraries like @tt{multi-lang} and @tt{polyglot} address this
problem by writing Racket modules to disk for later processing. But
sometimes disk activity and the filesystem are interruptions. Tesurell
aims to minimize that.

Dancing between notations can also be really fun and productive for
creative types. Tesurell gives Racket the LaTeX-like ability to swap
out notations to some desired effect when writing.

@section{Guide}
If you can write Scribble, you can write Tesurell markup. They both
use @racketmodname[scribble/reader], and use @racket[(provide doc)] to
share content. If you run a Tesurell module directly using DrRacket or
the @litchar{racket} launcher, it will evaluate @racket[(write
doc)]. Each Tesurell module provides all bindings from
@racketmodname[racket/base], plus those in the @secref{reference}
section.

The differences are more interesting. Tesurell documents do not
prescribe any document semantics because other languages already do
that. It is up to you to assemble notations to your preference.

@subsection{Example: Trivial Case}
You can @racket[embed] a @litchar{#lang} and require
the module in the same document.

@codeblock|{
#lang tesurell

@embed['my-module]|{
#lang racket/base
(provide out)
(define out 1)
}|

@require['my-module]
@out
}|

The following interaction holds:

@racketinput[doc]
@racketresult['(1)]

Here you can see that @racket[doc] reflects the content.

@subsection{Example: Defining your own Document}

If you want to define @racket[doc] youself, then
define a @racket[make-doc] procedure to create it.
You do not need to @racket[provide] the procedure.

@codeblock|{
#lang tesurell

Doesn't matter what gets written here.

@(define (make-doc elements)
  (printf "Normally: ~v~n" elements)
  "Overridden")
}|

The following interaction holds:

@racketinput[(require "markup.rkt")]
@racketresult[Normally: '("\n" "\n" "Doesn't matter what gets written here." "\n" "\n" |#<void>| "\n")]
@racketinput[doc]
@racketresult["Overridden"]

Here you can see the body before it gets cleaned up. The @racket[void]
value is what the @racket[(define (make-doc) ...)] evaluated to within
the document, and the newlines come from the Scribble reader.

This feature is useful as a simple way for documents to define
their own layout, namely without needing a templating system.

@subsection{Example: Using Other Language Features}

You can borrow more established languages and compose their
output.

@codeblock|{
#lang tesurell

@embed['other-a]|{
#lang scribble/manual
@title{Manual A}
}|
@embed['other-b]|{
#lang scribble/manual
@title{Manual B}
}|

@require[@rename-in['other-a [doc a]]]
@require[@rename-in['other-b [doc b]]]
@(define (make-doc . _) (list a b))
}|

@subsection{Example: Self-hosting}
Tesurell can self-host in the sense that it can recognize
@litchar{#lang tesurell} subdocuments and any extensions you may wish
to install. Note that a Tesurell subdocument cannot see anything in
the containing Tesurell document.

You could get around that by interpolating code within a subdocument,
but using string interpolation to build code can be dangerous.
Do not use Tesurell subdocuments as an alternative to Racket's
built-in means to define new @litchar{#lang}s.

Here's an example of a subdocument that overrides @racket[doc], while
the parent document uses the default representation of @racket[doc].

@codeblock|{
#lang tesurell

Gonna get meta.

@embed['other 'doc]|{
#lang tesurell @require[racket/string]

@(define (make-doc raw)
   (list 'pre
         (string-trim (string-join (filter string? raw) ""))))

Preformatted
      text
   document
}|
}|

The following interaction holds:

@racketinput[doc]
@racketresult['("Gonna get meta." (pre "Preformatted\n      text\n   document"))]

@subsection{Example: Inline Language Demo}
Since Tesurell supports inline Racket modules, you can also use it to
define new languages for immediate demonstration. Despite my earlier
warning, this example leverages string interpolation to provide input
to the example sum language.

@codeblock|{
#lang tesurell
@require[racket/list racket/format]

@define[N 100]

@embed['sum-lang]|{
#lang racket
(require syntax/strip-context)

(provide (rename-out [seq-read read]
                     [seq-read-syntax read-syntax]))

(define (seq-read in)
  (syntax->datum (seq-read-syntax #f in)))

(define (seq-read-syntax src in)
  (with-syntax ([operands (read in)])
    (strip-context
     #'(module container racket
         (provide message)
         (define message (foldl + 0 operands))))))
}|

Welcome to the most offensively contrived way to sum
the first @N positive integers to
@embed['show-off 'message]{
#lang reader 'sum-lang @(~v (range 1 (+ N 1)))}
}|

The following interaction holds:

@racketinput[doc]
@racketresult[
'("Welcome to the most offensively contrived way to sum the first"
            100
            "positive integers to"
            5050)]


@section{Markup Semantics}

Each @litchar{#lang tesurell} module has the following bindings available:

@itemlist[
@item{@racket[$src]: a complete path to the document being evaluated on the file system.}
@item{@racket[$raw]: a list containing data produced by evaluating the markup read from the input source code.}
@item{@racket[$module-namespace]: the namespace of the module impacted by the evaluated code.}
]

@section[#:tag "reference"]{Reference}

@defproc[(module/port [id symbol?] [autorequire symbol?] [in input-port?] [ns namespace? (current-namespace)]) any/c]{
Reads the module as @litchar{#lang}-prefixed source code from
@racket[in], such that @racket[(require id)] will work in
the given namespace.

If @racket[autorequire] is a symbol, then @racket[module/port]
will return the value bound to @racket[autorequire] by the
input module. Otherwise, @racket[module/port] will return
@racket[(void)].

@bold{BEWARE:} Like @racketmodname[racket/load], the modules defined
here are evaluated dynamically and are therefore not compiled. Two modules
defined by this procedure cannot @racket[require] each other via this
form. Unlike @racketmodname[racket/load], however, the modules can
@racket[provide] bindings. For best results, only use this for small
expressions of code that are not shared by other documents.
}

@defproc[(embed [id symbol?] [autorequire (or/c symbol? #f) #f] [str string?] ...) any/c]{
This is a markup-friendly form of @racket[module/port].

@codeblock|{
#lang tesurell

@embed['my-module 'data]|{
#lang racket/base
(provide data)
(define data "I am from an inline module.")
}|}|}

@defproc[(reformat-doc [doc (listof any/c)]) (listof any/c)]{
This procedure acts as the default @racket[make-doc] implementation
if one is not provided by a @racketmodname[tesurell] module.

The default value reduces noise from the Scribble reader by doing the
following in order:

@itemlist[#:style 'ordered
@item{Filters out all void values}
@item{Combines strings like so:
@racketblock[
(filter-map (λ (x) (and (not (equal? "" x))
                        (regexp-replace* #px"\\s\\s*" x " ")))
  (regexp-split #px"\n\n+"
    (string-trim (string-join strings ""))))]}]


In English, this combines the strings into one big string, and then trims
the excess whitespace off the ends. It will then split the
big string at each sequence of 2+ consecutive newlines. Each
resulting substring then has all sequences of at least one space
transformed into a single blank space.

The following interaction holds:

@racketinput[
(reformat-doc '("\n" "\n"  "Welcome   to the " "\nThunderdome"
                "\n" "\n" "\n" "\n\nOver " 1000 " masters blasted."))
]
@racketresult[
'("Welcome to the Thunderdome" "Over " 1000 " masters blasted.")
]

Under this interpretation, a paragraph is terminated by either the end
of the list or a contiguous string element. If you wish to preserve
the formatting of some part of a document, then you will need to wrap
it in some container to prevent @racket[reformat-doc] from changing it.

Also, zero or one space may appear after non-string values depending
on how that string was formatted in the markup.

@racketinput[
(reformat-doc '(1000 "km")) (code:comment "@|1000|km")
]
@racketresult[
'(1000 "km")
]

@racketinput[
(reformat-doc '(1000 "   meters")) (code:comment "@1000   meters")
]
@racketresult[
'(1000 " meters")
]
}
