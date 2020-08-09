;;;; package.lisp

(defpackage #:gemtext
  (:nicknames #:cl-gemtext)
  (:use #:cl)
  (:import-from #:alexandria
                #:curry
                #:starts-with-subseq
                #:with-gensyms
                #:when-let)
  (:export #:*gemtext-input*
           #:link
           #:heading
           #:listitem
           #:blockquote
           #:verbatim
           #:gt-href
           #:gt-label
           #:gt-level
           #:gt-text
           #:gt-alt
           #:decode-gemtext
           #:decode-gemtext-from-string))
