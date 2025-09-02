(in-package :cltpt/base)

;; holds all text formats
(defvar *text-formats* nil)

(defvar *special-text-object-names*
  '(anchor link task))

(defvar *format-alias-alist*
  '(("org" . "org-mode")
    ("tex" . "latex"))
  "an alist of (alias . format-name).")

(defclass text-format ()
  ((name
    :initarg :name
    :accessor text-format-name
    :documentation "the unique name of the text format, it is used to identify the format from the commandline (among other places).")
   (text-object-types
    :initarg :text-object-types
    :accessor text-format-text-object-types
    :documentation "the `text-object' subclasses that the format corresponds to.")
   (document-type
    :initarg :document-type
    :accessor text-format-document-type
    :documentation "the `text-document' (sub)class that corresponds to this format.")
   (special-text-objects
    :initarg :special-text-objects
    :accessor text-format-special-text-objects
    :documentation "special text objects are shared across formats and serve predefined purposes, like an anchor link or a reference. this serves as a map from `*special-text-object-names*' to the corresponding `text-object' for this format.")))

(defgeneric text-format-convert (src-text-format dest-text-format text)
  (:documentation "convert from one text format into another."))

(defun make-text-format (name &optional text-object-types (doc-type 'document))
  (let ((fmt (make-instance 'text-format))
        (existent (text-format-by-name name)))
    (when existent
      (setf *text-formats* (delete existent *text-formats*)))
    (push fmt *text-formats*)
    (setf (text-format-name fmt) name)
    (setf (text-format-text-object-types fmt) text-object-types)
    (setf (text-format-document-type fmt) doc-type)
    fmt))

(defmethod text-format-convert ((fmt1 text-format) (fmt2 text-format) text)
  "convert TEXT from FMT1 to FMT2."
  (convert-text fmt1 fmt2 text))

(defun parse-file (filepath fmt)
  "takes a FILEPATH and a `text-format' FMT, returns the parsed object tree."
  (let* ((text (uiop:read-file-string filepath))
         (text-tree (parse text
                           (text-format-text-object-types fmt)
                           :doc-type (text-format-document-type fmt))))
    text-tree))

(defun text-format-by-name (name)
  "takes the name of a `text-format', returns the object."
  (find name *text-formats* :key 'text-format-name :test 'string=))

(defgeneric text-format-escape (fmt text escapable-chars escape-newlines)
  (:documentation "handles special characters when converting across formats.

- FMT: destination `text-format' we are converting to,
- TEXT: string for escaping,
- ESCAPABLE-CHARS: an alist mapping a char to an escape sequence, see `cltpt/latex:*latex-escape-table*',
- ESCAPE-NEWLINES: whether to escape newlines too."))

;; default one
(defmethod text-format-escape ((fmt text-format)
                               text
                               escapable-chars
                               escape-newlines)
  text)

(defun text-format-from-alias (alias)
  "utility function to grab a text format using its alias. the aliases are hard-coded."
  (text-format-by-name
   (cdr (assoc alias *format-alias-alist* :test 'equal))))