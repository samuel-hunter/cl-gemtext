;;;; package.lisp

(defpackage #:gemtext
  (:documentation
   "Decode the Gemini Protocol's gemtext markup into structured data.")
  (:nicknames #:cl-gemtext)
  (:use #:cl #:alexandria)
  (:export #:+gemini-version+
           #:*gemtext-input*
           ;; classes
           #:gt-link
           #:gt-heading
           #:gt-listitem
           #:gt-blockquote
           #:gt-preformatted
           ;; accessors
           #:gt-href
           #:gt-label
           #:gt-alt
           #:gt-level
           #:gt-text
           ;; decoding
           #:decode-gemtext-line
           #:decode-gemtext-line-from-string
           #:decode-gemtext
           #:decode-gemtext-from-string))
