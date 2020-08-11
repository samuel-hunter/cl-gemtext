# cl-gemtext
### _Samuel Hunter_

A library for decoding the Gemini Protocol's gemtext markup into
structured data.

Project Gemini is a semi-recent internet protocol protject that
describes itself as somewhere between Gopher and the convnetional
Web. Much like how HTML is the native respones format for HTTP, Gemini
houses its own native response -- a markup format named "gemtext",
which is parsed line-by-line. If you're unfamiliar with Project Gemini
and want to learn more, I recommend reading the de-facto homepage in
[HTTP](https://gemini.circumlunar.space/) or in
[Gemini](gemini://gemini.circumlunar.space/) [(Mozz.us
Proxy)](https://portal.mozz.us/gemini/gemini.circumlunar.space/).

You could use this protocol if you would like to develop a gemini
client to format gemtext pages into rich text. The specification was
small enough that I was able to read it and implement a parser in a
couple days of hacking, but this is still a brand spanking new
library, bound to sport a few hidden bugs. Feel free to issue a report
for behavioral bugs, incorrect/unclear documentation, or send over a
pull request for anything you think would improve it.

## Quick Usage

Drop this project in a place that ADSF can see it - I personally
develop this library in `~/quicklisp/local-projects/` so that you can
use quicklisp to pull in its dependencies, `alexandria` and `cl-unicode`.

```lisp
* (require :cl-gemtext)
* (use-packge :gemtext)

;; Read the first line from a stream
* (with-input-from-string (stream
    "=>gemini://gemini.circumlunar.space/ Hello World!")
    (decode-gemtext-line stream))
#<GT-LISTITEM HREF="gemini://gemini.circumlunar.space/" LABEL="Hello world!">
* (values (gt-href *) (gt-label *)) ;; Access its attributes via gt-*
"gemini://gemini.circumlunar.space/"
"Hello world!"

;; Its default stream is the exported *gemtext-input*, which by
;; default is a synonym string to *standard-input*:
* (with-input-from-string (*gemtext-input* "> How about a quote?")
    (decode-gemtext-line))
#<GT-QUOTE TEXT="How about a quote?">

;; Read from a string
* (decode-gemtext-line-from-string "# Headings!")
#<GT-HEADING LEVEL=1 TEXT="Headings!">

;; Regular text lines are returned as a standard string.
* (decode-gemtext-line-from-string "Just regular text.")
"Just regular text."

;; Read an entire document with decode-gemtext or
;; decode-gemtext-from-string
* (decode-gemtext-from-string "# A heading
## A sub-heading
### A sub-sub-heading")
(#<GT-HEADING LEVEL=1 "A heading">
 #<GT-HEADING LEVEL=2 "A sub-heading">
 #<GT-HEADING LEVEL=3 "A sub-sub-heading">)
 
;; Finally, remove automatic whitespace trimming:
* (decode-gemtext-from-string ">   Lots of whitespace   ")
#<GT-QUOTE TEXT="Lots of whitespace">
* (decode-gemtext-from-string ">   Lots of whitespace   " nil)
#<GT-QUOTE TEXT="   Lots of whitespace   ">
```
