(defpackage :cltpt
  (:use :cl))
(in-package :cltpt)

(defclass text-object ()
  ((properties
    :initarg :properties
    :accessor text-object-properties
    :documentation "other properties that the cltpt text-object may hold.")
   (text
    :initarg :text
    :accessor text-object-text
    :documentation "the text that the element corresponds to.")
   (id
    :initarg :id
    :accessor text-object-id
    :documentation "a unique string identifying this text object (in text files).")
   (children
    :initarg :children
    :accessor text-object-children
    :documentation "the children elements of this element."
    :initform nil)
   (opening-macro
    :initarg :opening-macro
    :accessor text-object-opening-macro
    :documentation "the macro that starts the object")
   (closing-macro
    :initarg :opening-macro
    :accessor text-object-closing-macro
    :documentation "the macro that ends the object"
    :initform nil)
   (is-self-contained
    :initarg :is-self-contained
    :accessor text-object-is-self-contained
    :documentation "whether the text object can contain/enclose other text or not."
    :initform nil))
  (:documentation "cl-text objects base class"))

(defgeneric text-object-init (text-obj str1 opening-macro closing-macro)
  (:documentation "this function is invoked by the parser,
`str1' is the string (or buffer) being parsed, `opening-macro' is the macro from `str1'
that resulted in this object being constructed."))

(defgeneric text-object-ends-by (text-obj value)
  (:documentation "should return whether the value indicates the ending of the object's
region. you should just make it return a symbol like `end-type'."))

;; the default end function returns the value 'end, which should end any text object that
;; comes before it, this isnt recommended as it may cause ambiguations
;; value can be 'end-of-buffer to denote the end of file/buffer/text, this is useful for headers
;; which should be ended by new headers but also by the end of the text
;; value can also be another text object
(defmethod text-object-ends-by ((text-obj t) value)
  (and (symbolp value) (string= value 'end)))

(defgeneric text-object-export (text-obj backend)
  (:documentation "function that takes a cltpt text-object and exports it to the specificed backend. this function is invoked when exporting, it should return two values, a string and a boolean indicating whether to handle the exporting of its children or not."))

(defmethod text-object-export ((text-obj t) backend)
  "default export function will just return the text contents between the opening and closing macros, and `t' to indiciate that we want the exporter to handle the exporting of its children"
  (values (text-object-text text-obj) t))

;; default parsing function will just set the text slot of the object
(defmethod text-object-init ((text-obj t) str1 opening-macro closing-macro)
  (setf (text-object-opening-macro text-obj) opening-macro)
  (setf (text-object-closing-macro text-obj) closing-macro)
  (if closing-macro
      (setf (text-object-text text-obj)
            (subseq str1
                    (region-end (text-macro-region opening-macro))
                    (region-begin (text-macro-region closing-macro))))
      (setf (text-object-text text-obj) (format nil "~A" (text-macro-result opening-macro)))))

(defmethod text-object-begin ((text-obj t))
  "where the text object begins relative to its parent."
  (region-begin (text-macro-region (text-object-opening-macro text-obj))))

(defmethod text-object-end ((text-obj t))
  "where the text object ends relative to its parent."
  (if (text-object-closing-macro text-obj)
      (region-end (text-macro-region (text-object-closing-macro text-obj)))
      (region-end (text-macro-region (text-object-opening-macro text-obj)))))

(defclass document (text-object)
  ()
  (:documentation "top-level text element."))

(defun make-document (text children)
  (make-instance 'document :text text :children children))

(defclass text-block (text-object)
  ()
  (:documentation "a text block."))

(defun make-block (&key &allow-other-keys)
  (make-instance 'text-block))

(defmethod text-object-export ((obj text-block) backend)
  (when (equal backend 'latex)
    "cap")
  (text-object-text obj))

;; aliases for blocks
(setf (symbol-function 'b) (symbol-function 'make-block))

(defmethod text-object-ends-by ((text-obj text-block) value)
  (and (symbolp value) (string= value 'end-block)))

;; the default object for macros that dont evaluate to a text-object
(defclass default-text-object (text-object)
  ()
  (:documentation "a trivial object."))

(defmethod text-object-sorted-children (text-object)
  "return the children of the text-obj, sorted by starting point."
  (sort
   (text-object-children text-object)
   '<
   :key
   (lambda (obj)
     (region-begin (text-macro-region (text-object-opening-macro obj))))))

(defmethod text-object-contents-begin ((text-obj t))
  "where the text object begins relative to its parent."
  (if (text-object-closing-macro text-obj)
      (region-end (text-macro-region (text-object-opening-macro text-obj)))
      0))

(defmethod text-object-contents-end ((text-obj t))
  "where the text object ends relative to its parent."
  (if (text-object-closing-macro text-obj)
      (region-end (text-macro-region (text-object-closing-macro text-obj)))
      0))