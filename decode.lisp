;;;; decode.lisp

(in-package #:gemtext)


(defvar *gemtext-input* (make-synonym-stream '*standard-input*))

;; read helper function

(defvar +whitespacep+ (cl-unicode:property-test "whitespace")
  "Whitespace character predicate")

(defun read-until (test &optional (stream *standard-input*) (eof-error-p t) eof-value)
  (loop :with result := (make-array 0 :element-type 'character :adjustable t)
        :until (funcall test (peek-char nil stream eof-error-p eof-value))
        :do (vector-push-extend (read-char stream) result)
        :finally (return (copy-seq result)) ;; return a non-adjustable copy of the string
        ))

;; special decoders

(defun decode-link (line)
  "Decode a gemtext link from a prefix-stripped LINE."
  (with-input-from-string (stream line)
    (peek-char t stream) ;; skip all whitepsace up until url
    (let* ((href (read-until +whitespacep+ stream
                             nil #\Newline)))
      ;; skip more whitespace and read the label. If EOF, return early
      ;; with no label.
      (handler-case
          (progn
            (peek-char t stream)
            ;; BUG: (read-line) will include whitespace at the end of
            ;; the line. Perhaps create a helper
            ;; `string-right-trim-if' function?
            (link href (read-line stream)))
        (end-of-file () (link href))))))

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
