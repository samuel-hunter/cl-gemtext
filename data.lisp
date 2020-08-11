;;;; data.lisp

(in-package #:gemtext)

;; Direct quote from
;; gemini://gemini.circumlunar.space/docs/specification.gmi
;; as of 2020-07-02 (visited 2020-08-10):

;; This is an increasingly less rough sketch of an actual spec for
;; Project Gemini.  Although not finalised yet, further changes to the
;; specification are likely to be relatively small.  You can write
;; code to this pseudo-specification and be confident that it probably
;; won't become totally non-functional due to massive changes next
;; week, but you are still urged to keep an eye on ongoing development
;; of the protocol and make changes as required.
(defvar +gemini-version+ '(0 14 2)
  "The major version, minor version, and patchlevel of the latest-supported gemini protocol.")

(defclass gt-link ()
  ((href :type string :initarg :href :accessor gt-href)
   (label :type string :initform "" :initarg :label :accessor gt-label)))

(defmethod print-object ((object gt-link) stream)
  (with-slots (href label) object
    (format stream "#<~S HREF=~S LABEL=~S>" 'gt-link href label)))


(defclass gt-preformatted ()
  ((alt :type string :initform "" :initarg :alt :accessor gt-alt)
   (text :type string :initarg :text :accessor gt-text)))

(defmethod print-object ((object gt-preformatted) stream)
  (format stream "#<~S ALT=~S TEXT=~S>" 'gt-preformatted (gt-alt object) (gt-text object)))


(defclass gt-heading ()
  ((level :type (integer 1 3) :initarg :level :accessor gt-level)
   (text :type string :initarg :text :accessor gt-text)))

(defmethod print-object ((object gt-heading) stream)
  (with-slots (level text) object
    (format stream "#<~S LEVEL=~D TEXT=~S>" 'gt-heading level text)))


(defclass gt-listitem ()
  ((text :type string :initarg :text :accessor gt-text)))

(defmethod print-object ((object gt-listitem) stream)
  (format stream "#<~S TEXT=~S>" 'gt-listitem (gt-text object)))


(defclass gt-quote ()
  ((text :type string :initarg :text :accessor gt-text)))

(defmethod print-object ((object gt-quote) stream)
  (format stream "#<~S TEXT=~S>" 'gt-quote (gt-text object)))
