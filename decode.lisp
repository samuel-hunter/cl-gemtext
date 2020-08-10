;;;; decode.lisp

(in-package #:gemtext)


(defvar *gemtext-input* (make-synonym-stream '*standard-input*))

;; string/read helper functions

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

(defun trim-whitespace (string)
  (let* ((start (position-if-not +whitespacep+ string))
         (end (and start (1+ (position-if-not +whitespacep+ string
                                              :from-end t :start start)))))
    (if start
        (subseq string start end)
        "")))

(declaim (inline trim-when))
(defun trim-if (bool string)
  (if bool
      (trim-whitespace string)
      string))

;; decoders

(defun decode-link (trim-whitespace-p)
  (skip-whitespace *gemtext-input*)
  (let ((href (read-until +whitespacep+ *gemtext-input* nil #\Newline)))
    (handler-case
        (progn
          (skip-whitespace *gemtext-input*)
          (make-instance 'gt-link
                         :href href
                         :label (trim-if trim-whitespace-p
                                         (read-line *gemtext-input* nil ""))))
      (end-of-file () (make-instance 'gt-link :href href)))))

(defun decode-preformatted (trim-whitespace-p)
  (loop :with alt := (trim-if trim-whitespace-p
                              (read-line *gemtext-input* nil ""))
        :with result := ""
        :for (line missing-newline-p)
          := (multiple-value-list (read-line *gemtext-input* nil "```"))
        :until (starts-with-subseq "```" line)
        :do (setf result (concatenate 'string result line
                                      (unless missing-newline-p '(#\Newline))))
        :finally (return (make-instance 'gt-preformatted
                                        :alt alt
                                        :text result))))

(defvar +gemtext-max-heading-level+ 3)

(defun decode-heading (trim-whitespace-p)
  (loop :with level := 1
        :repeat (1- +gemtext-max-heading-level+)
        :while (char= (peek-char nil *gemtext-input* nil #\Newline) #\#)
        :do (progn (incf level)
                   (read-char *gemtext-input*))
        :finally (progn
                   (skip-whitespace *gemtext-input*)
                   (return (make-instance
                            'gt-heading
                            :level level
                            :text (trim-if trim-whitespace-p
                                           (read-line *gemtext-input* nil "")))))))

(defun decode-listitem (trim-whitespace-p)
  (make-instance 'gt-listitem
                 :text (trim-if trim-whitespace-p
                                (read-line *gemtext-input* nil ""))))

(defun decode-quote (trim-whitespace-p)
  (make-instance 'gt-quote
                 :text (trim-if trim-whitespace-p
                                (read-line *gemtext-input* nil ""))))

;; Prefix table

;; "It is possible to unambiguously determine a line's type purely by
;; inspecting its first three characters" - gemini specification
(defvar +prefix-max-length+ 3)

(defparameter *prefix-table*
  (plist-hash-table
   '("=>" decode-link
     "```" decode-preformatted
     "#" decode-heading
     "* " decode-listitem
     ">" decode-quote)
   :test 'equal))

(defun decoder (prefix)
  (gethash prefix *prefix-table*))

;; Dispatch gemtext decoding

(defun decode-gemtext (&optional (stream *gemtext-input*) (trim-whitespace-p t))
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
                (return (funcall decoder trim-whitespace-p)))
              ;; If no prefix is found, read the rest of the line and
              ;; return the result.
          :finally (return (trim-if trim-whitespace-p
                                    (concatenate 'string prefix
                                                 (read-line *gemtext-input*)))))))

(defun decode-gemtext-from-string (string &optional (trim-whitespace-p t))
  "Read a line of gemtext from STRING and return the corresponding structured object."
  (with-input-from-string (stream string)
    (decode-gemtext stream trim-whitespace-p)))

(defun decode-gemtext-lines (&optional (stream *gemtext-input*) (trim-whitespace-p t))
  "Keep reading lines from STREAM until END-OF-LINE is signaled, and then
return a list of the corresponding objects."
  (let (lines)
    (handler-case
        (loop (push (decode-gemtext stream trim-whitespace-p) lines))
      (end-of-file () (reverse lines)))))

(defun decode-gemtext-lines-from-string (string &optional (trim-whitespace-p t))
  "Read all lines from STRING and then return a list of the corresponding objects."
  (with-input-from-string (stream string)
    (decode-gemtext-lines stream trim-whitespace-p)))
