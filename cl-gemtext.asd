;;;; cl-gemtext.asd

(asdf:defsystem #:cl-gemtext
  :description "Decode the Gemini Protocol's gemtext markup into structuerd data."
  :author "Samuel Hunter"
  :license  "BSD 3-Clause"
  :version "0.0.1"
  :serial t
  :components ((:file "package")
               (:file "data")
               (:file "gemtext")))
