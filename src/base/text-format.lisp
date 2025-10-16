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

(defmethod convert-text ((fmt1 text-format) (fmt2 text-format) text)
  "convert TEXT from FMT1 to FMT2."
  (let* ((text-tree (parse fmt1 text))
         (result (convert-tree text-tree fmt1 fmt2)))
    result))

(defmethod convert-file ((fmt1 text-format)
                         (fmt2 text-format)
                         src-file
                         dest-file)
  (let* ((text (cltpt/file-utils:read-file src-file))
         (doc (parse fmt1 text))
         (result (convert-document fmt1 fmt2 doc)))
    (cltpt/file-utils:write-file dest-file result)))

(defmethod parse-file ((fmt text-format) filepath)
  "takes a FILEPATH and a `text-format' FMT, returns the parsed object tree."
  (let* ((text (cltpt/file-utils:read-file filepath))
         (text-tree (parse fmt text)))
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

(defgeneric text-format-conversion-template (fmt)
  (:documentation "a template for use when converting a document from some format into FMT."))

(defmethod text-format-conversion-template ((fmt t))
  nil)

;; just a dummy format used mainly for conversion with macros
(defvar *simple-format*
  (make-text-format "simple" '(post-lexer-text-macro text-macro))
  "an instance of `cltpt/base:text-object' for a text format with macros only.")

(defmethod cltpt/base:text-format-conversion-template ((fmt (eql *simple-format*)))
  "%(getf cltpt/base:*convert-info* :text-obj)")