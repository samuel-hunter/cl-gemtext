;;;; gemtext.lisp

(in-package #:gemtext)


(defvar *gemtext-input* (make-synonym-stream '*standard-input*))

(defun decode-gemtext (&optional (stream *gemtext-input*))
  "Read a line of gemtext from STREAM and return the corresponding structured object."
  (declare (ignore stream))
  (error "Not implemented."))

(defun decode-gemtext-from-string (string)
  "Read a line of gemtext from STRING and return the corresponding structured object."
  (with-input-from-string (stream string)
    (decode-gemtext stream)))
