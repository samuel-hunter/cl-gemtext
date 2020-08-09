;;;; decode.lisp

(in-package #:gemtext)


(defvar *gemtext-input* (make-synonym-stream '*standard-input*))


;; special decoders

(defun decode-link (line)
  (declare (ignore line))
  (error "Can't decode links yet"))

(defun decode-verbatim (alt)
  (declare (ignore alt))
  (error "Can't decode verbatim yet"))

;; decode dispatch

(defparameter *decoder-table* ())

(defmacro push-prefix-dispatches (&body dispatches)
  `(progn
     ,@(loop :for (prefix function) :in dispatches
             :collect `(push (cons ,prefix ,function) *decoder-table*))))

(push-prefix-dispatches
  ("=>" 'decode-link)
  ("# " (curry 'heading 1))
  ("## " (curry 'heading 2))
  ("### " (curry 'heading 3))
  ("* " 'listitem)
  ("```" 'decode-verbatim))

(defun find-decoder (line)
  (dolist (dispatch-pair *decoder-table*)
    (multiple-value-bind (starts-with-p suffix)
        (starts-with-subseq (car dispatch-pair) line :return-suffix t)
      (when starts-with-p
        (return-from find-decoder (values (cdr dispatch-pair) suffix)))))
  (values nil nil))

(defun decode-gemtext (&optional (stream *gemtext-input*) (eof-error-p t) eof-value)
  "Read a line of gemtext from STREAM and return the corresponding structured object."
  (let ((line (read-line stream eof-error-p eof-value)))
    (multiple-value-bind (decoder line-suffix) (find-decoder line)
      (if decoder
          (funcall decoder line-suffix)
          line))))

(defun decode-gemtext-from-string (string)
  "Read a line of gemtext from STRING and return the corresponding structured object."
  (with-input-from-string (stream string)
    (decode-gemtext stream)))
