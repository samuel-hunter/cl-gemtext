;;;; decode.lisp

(in-package #:gemtext)


(defvar *gemtext-input* (make-synonym-stream '*standard-input*))

;; read helper function

(defvar +whitespacep+ (cl-unicode:property-test "whitespace")
  "Whitespace character predicate")

;; helper function to take out the (funcall) if called directly
(declaim (inline whitespacep))
(defun whitespacep (char)
  (funcall +whitespacep+ char))

(defun read-until (test stream &optional (eof-error-p t) eof-value)
  (loop :with result := (make-array 0 :element-type 'character
                                      :adjustable t)
        :until (funcall test (peek-char nil stream eof-error-p eof-value))
        :do (vector-push-extend (read-char stream) result)
            ;; return a non-adjustable copy of the string
        :finally (return (copy-seq result))))

(defun skip-whitespace (stream)
  (read-until (lambda (char) (or (not (whitespacep char))
                                 (char= char #\Newline)))
              stream nil #\Newline)
  (values))

;; decoders

(defun decode-link ()
  (skip-whitespace *gemtext-input*)
  (let ((href (read-until +whitespacep+ *gemtext-input* nil #\Newline)))
    (handler-case
        (progn
          (skip-whitespace *gemtext-input*)
          (make-instance 'gt-link
                         :href href
                         :label (read-line *gemtext-input*)))
      (end-of-file () (make-instance 'gt-link :href href)))))

(defun decode-preformatted ()
  (loop :with alt := (read-line *gemtext-input* nil "")
        :with result := ""
        :for (line missing-newline-p)
          := (multiple-value-list (read-line *gemtext-input* nil "```"))
        :until (starts-with-subseq "```" line)
        :do (setf result (concatenate 'string result line
                                      (unless missing-newline-p '(#\Newline))))
        :finally (return (make-instance 'gt-preformatted
                                        :alt alt
                                        :text result))))

;; Prefix table

;; "It is possible to unambiguously determine a line's type purely by
;; inspecting its first three characters" - gemini specification
(defvar +prefix-max-length+ 3)

(defparameter *prefix-table*
  (plist-hash-table
   '("=>" decode-link
     "```" decode-preformatted)
   :test 'equal))

(defun decoder (prefix)
  (gethash prefix *prefix-table*))

;; Dispatch gemtext decoding

(defun decode-gemtext (&optional (stream *gemtext-input*))
  "Read a line of gemtext from STREAM and return the corresponding structured object.

If TRIM-STRING-P, trim the text of any leading or trailing whitespace."
  (let ((*gemtext-input* stream))
    (loop :with prefix := (make-array 3 :element-type 'character :fill-pointer 0)
          :repeat +prefix-max-length+
          :for char := (read-char *gemtext-input*)
            :then (read-char *gemtext-input* nil #\Newline)
          ;; Return everything read as text if a newline is found early.
          :when (char= char #\Newline)
            :return (copy-seq prefix)
          ;; If a decoder is found, return the result of that.
          :do (vector-push char prefix)
          :do (when-let ((decoder (decoder prefix)))
                (return (funcall decoder)))
              ;; If no prefix is found, read the rest of the line and
              ;; return the result.
          :finally (return (concatenate 'string prefix (read-line *gemtext-input*))))))

(defun decode-gemtext-from-string (string)
  "Read a line of gemtext from STRING and return the corresponding structured object."
  (with-input-from-string (stream string)
    (decode-gemtext stream)))
