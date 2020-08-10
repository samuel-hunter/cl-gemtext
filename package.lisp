;;;; package.lisp

(defpackage #:gemtext
  (:documentation "Decode the Gemini Protocol's gemtext markup into structured data.")
  (:nicknames #:cl-gemtext)
  (:use #:cl #:alexandria)
  (:export #:*gemtext-input*
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
           ;; coding
           #:decode-gemtext
           #:decode-gemtext-from-string))
