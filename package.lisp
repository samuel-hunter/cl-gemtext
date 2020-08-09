;;;; package.lisp

(defpackage #:gemtext
  (:nicknames #:cl-gemtext)
  (:use #:cl)
  (:export #:*gemtext-input*
           #:link
           #:heading
           #:listitem
           #:verbatim
           #:gt-href
           #:gt-label
           #:gt-level
           #:gt-text
           #:gt-alt
           #:decode-gemtext
           #:decode-gemtext-from-string))
