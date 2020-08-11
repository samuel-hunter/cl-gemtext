;;;; cl-gemtext.asd

(asdf:defsystem #:cl-gemtext
  :description "Decode the Gemini Protocol's gemtext markup into structured data."
  :author "Samuel Hunter"
  :license  "BSD 3-Clause"
  :version "0.1.0"
  :depends-on (#:alexandria
               #:cl-unicode)
  :serial t
  :components ((:file "package")
               (:file "data")
               (:file "decode")))
