;;;; data.lisp

(in-package #:gemtext)


(defclass link ()
  ((href :type string :initarg :href :accessor gt-href)
   (label :type string :initform "" :initarg :label :accessor gt-label)))

(defmethod print-object ((object link) stream)
  (with-slots (href label) object
    (format stream "#<~S HREF=~S LABEL=~S>" 'link href label)))


(defclass heading ()
  ((level :type (integer 1 3) :initarg :level :accessor gt-level)
   (text :type string :initarg :text :accessor gt-text)))

(defmethod print-object ((object heading) stream)
  (with-slots (level text) object
    (format stream "#<~S LEVEL=~D TEXT=~S" 'heading level text)))


(defclass listitem ()
  ((text :type string :initarg :text :accessor gt-text)))

(defmethod print-object ((object listitem) stream)
  (format stream "#<~S TEXT=~S>" 'listitem (gt-text object)))


(defclass blockquote ()
  ((text :type string :initarg :text :accessor gt-text)))

(defmethod print-object ((object blockquote) stream)
  (format stream "#<~S TEXT=~S>" 'blockquote (gt-text object)))


(defclass verbatim ()
  ((alt :type string :initform "" :initarg :alt :accessor gt-alt)
   (text :type string :initarg :text :accessor gt-text)))

(defmethod print-object ((object verbatim) stream)
  (format stream "#<~S ALT=~S TEXT=~S>" 'verbatim (gt-alt object) (gt-text object)))
