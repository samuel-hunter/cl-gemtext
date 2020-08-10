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

;; string helper functions

(defun string-left-trim-if-position (test string)
  (or (position-if (complement test) string)
      (length string)))

(defun string-right-trim-if-position (test string &optional (start 0))
  (1+ (or (position-if (complement test) string :from-end t :start start)
          (1- start))))

(defun string-left-trim-if (test string)
  (subseq string (string-left-trim-if-position test string)))

(defun string-right-trim-if (test string)
  (subseq string 0 (string-right-trim-if-position test string)))

(defun string-trim-if (test string)
  (let* ((start (string-left-trim-if-position test string))
         (end (string-right-trim-if-position test string start)))
    (subseq string start end)))

;; TODO (defun string-trim-if ...)

;; decoders

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
            (make-instance 'link :href href :label (read-line stream)))
        (end-of-file () (make-instance 'link :href href))))))

(defun decode-heading (level line)
  (make-instance 'heading :level level :text line))

(defun decode-listitem (line)
  (make-instance 'listitem :text line))

(defun decode-blockquote (line)
  "Decode a blockquote from a prefix-stripped LINE."
  (make-instance 'blockquote :text line))

(defun decode-verbatim (alt)
  (declare (ignore alt))
  (error "Can't decode verbatim yet"))

;; decode dispatch

(defmacro define-prefix-table (var &body dispatches)
  `(defparameter ,var
     (list ,@(loop :for (prefix function) :in dispatches
                   :collect `(cons ,prefix ,function)))))

(define-prefix-table *prefix-table* ;; (defvar *prefix-table* ...)
  ("=>" 'decode-link)
  ("# " (curry 'decode-heading 1))
  ("## " (curry 'decode-heading 2))
  ("### " (curry 'decode-heading 3))
  ("* " 'decode-listitem)
  (">" 'decode-blockquote)
  ("```" 'decode-verbatim))

(defun find-decoder (line)
  (dolist (dispatch-pair *prefix-table*)
    (multiple-value-bind (starts-with-p suffix)
        (starts-with-subseq (car dispatch-pair) line :return-suffix t)
      (when starts-with-p
        (return-from find-decoder (values (cdr dispatch-pair) suffix)))))
  (values nil nil))

(defun decode-gemtext (&optional (stream *gemtext-input*) (trim-string-p t)
                         (eof-error-p t) eof-value)
  "Read a line of gemtext from STREAM and return the corresponding structured object.

If TRIM-STRING-P, trim the text of any leading or trailing whitespace."
  (let ((line (read-line stream eof-error-p eof-value)))
    (multiple-value-bind (decoder line-suffix) (find-decoder line)
      (if decoder
          ;; call the decoder if a prefix is found.
          (funcall decoder
                   (if trim-string-p
                       (string-trim-if +whitespacep+ line-suffix)
                       line-suffix))
           ;; otherwise, return the text as-is.
          (if trim-string-p
              (string-trim-if +whitespacep+ line)
              line)))))

(defun decode-gemtext-from-string (string &optional (trim-string-p t)
                                            (eof-error-p t) eof-value)
  "Read a line of gemtext from STRING and return the corresponding structured object."
  (with-input-from-string (stream string)
    (decode-gemtext stream trim-string-p eof-error-p eof-value)))
