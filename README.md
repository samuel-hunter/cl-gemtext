# cl-gemtext
### _Samuel Hunter_

A library for decoding the Gemini Protocol's gemtext markup into
structured data.

Currently incomplete.

## Future Usage

```lisp
* (require :cl-gemtext)
* (decode-gemtext-line *standard-input*)
#<some form of gemtext structure>
* (decode-gemtext-line-from-string "Gemtext data")
#<some form of gemtext structure>
```

## License

BSD 3-Clause

