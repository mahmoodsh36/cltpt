(defpackage :cl-text
  (:use :cl))
(in-package :cl-text)

(defclass text-object ()
  ((properties
    :initarg :properties
    :accessor text-object-properties
    :documentation "properties of a cl-text object.")
   (buffer-begin
    :initarg :buffer-begin
    :accessor text-object-buffer-begin
    :documentation "the beginning point in the buffer that the element belongs to")
   (buffer-end
    :initarg :buffer-end
    :accessor text-object-buffer-end
    :documentation "the ending point of the element in the buffer it belongs to")
   (text
    :initarg :text
    :accessor text-object-text
    :documentation "the text that the element corresponds to.")
   (children
    :initarg :children
    :accessor text-object-children
    :documentation "the children elements of this element."))
  (:documentation "cl-text objects base class"))

(defclass text-document (cl-text-object)
  ()
  (:documentation "buffer-top-level text element."))

;; (defclass text-block ()
;;   ()
;;   )

;; (defgeneric text-object-parse (obj backend))

;; (defgeneric text-object-export (obj backend)
;;   (:documentation "function that takes a cl-text object and exports it to the specificed backend."))

;; (defmethod text-object-export ((elm texxt-block) backend)
;;   )

(defgeneric text-object-parse (str1 macro-begin macro-end)
  "this function is invoked by the parser, the object should mark its \"territory\" and setup values like where its content begins and ends (if relevant).
`str1' is the string (or buffer) being parsed, `macro-begin' marks the position at which the
macro that resulted in this object being constructed starts, and `macro-end' is the position
where it ends.
the function should tell the parser where it should next continue."
  )