;;;; data.lisp

(in-package #:gemtext)


(defclass gt-link ()
  ((href :type string :initarg :href :accessor gt-href)
   (label :type string :initform "" :initarg :label :accessor gt-label)))

(defmethod print-object ((object gt-link) stream)
  (with-slots (href label) object
    (format stream "#<~S HREF=~S LABEL=~S>" 'gt-link href label)))


(defclass gt-heading ()
  ((level :type (integer 1 3) :initarg :level :accessor gt-level)
   (text :type string :initarg :text :accessor gt-text)))

(defmethod print-object ((object gt-heading) stream)
  (with-slots (level text) object
    (format stream "#<~S LEVEL=~D TEXT=~S" 'gt-heading level text)))


(defclass gt-listitem ()
  ((text :type string :initarg :text :accessor gt-text)))

(defmethod print-object ((object gt-listitem) stream)
  (format stream "#<~S TEXT=~S>" 'gt-listitem (gt-text object)))


(defclass gt-blockquote ()
  ((text :type string :initarg :text :accessor gt-text)))

(defmethod print-object ((object gt-blockquote) stream)
  (format stream "#<~S TEXT=~S>" 'gt-blockquote (gt-text object)))


(defclass gt-verbatim ()
  ((alt :type string :initform "" :initarg :alt :accessor gt-alt)
   (text :type string :initarg :text :accessor gt-text)))

(defmethod print-object ((object gt-verbatim) stream)
  (format stream "#<~S ALT=~S TEXT=~S>" 'gt-verbatim (gt-alt object) (gt-text object)))
