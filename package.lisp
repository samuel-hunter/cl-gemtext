;;;; package.lisp

(defpackage #:gemtext
  (:documentation "Decode the Gemini Protocol's gemtext markup into structured data.")
  (:nicknames #:cl-gemtext)
  (:use #:cl)
  (:import-from #:alexandria
                #:curry
                #:starts-with-subseq
                #:with-gensyms
                #:when-let)
  (:export #:*gemtext-input*
           ;; classes
           #:gt-link
           #:gt-heading
           #:gt-listitem
           #:gt-blockquote
           #:gt-verbatim
           ;; accessors
           #:gt-href
           #:gt-label
           #:gt-level
           #:gt-text
           #:gt-alt
           ;; coding
           #:decode-gemtext
           #:decode-gemtext-from-string))
