(in-package :cltpt/base)

(defvar *format-alias-alist*
  '(("org" . "org-mode")
    ("tex" . "latex"))
  "an alist of (alias . format-name).")

(defun format-name-to-keyword (name)
  "map a format NAME (string) to a keyword, resolving aliases. signals an error if unknown."
  (let ((n (or (cdr (assoc name *format-alias-alist* :test #'equal)) name)))
    (cond ((string-equal n "html") :html)
          ((string-equal n "latex") :latex)
          ((string-equal n "org-mode") :org-mode)
          ((string-equal n "simple") :simple)
          ((string-equal n "dummy") :dummy)
          (t (error "unknown format name: ~A" name)))))

(defgeneric text-format-text-object-types (fmt)
  (:documentation "the `text-object' subclasses that the format corresponds to."))

(defgeneric text-format-document-type (fmt)
  (:documentation "the `text-document' (sub)class that corresponds to this format."))

(defmethod text-format-text-object-types ((fmt (eql :simple)))
  '(post-lexer-text-macro text-macro))

(defmethod text-format-document-type ((fmt (eql :simple)))
  'document)

(defmethod convert-text (fmt1 fmt2 text)
  "convert TEXT from FMT1 to FMT2."
  (let* ((text-tree (parse fmt1 text))
         (result (convert-tree text-tree fmt1 fmt2)))
    result))

(defmethod convert-file (fmt1
                         fmt2
                         src-file
                         dest-file)
  (let* ((doc (parse-file fmt1 src-file))
         (result (convert-document fmt1 fmt2 doc)))
    (cltpt/file-utils:write-file dest-file result)))

(defmethod parse-file (fmt filepath &key text-object-types)
  "takes a FILEPATH and a format FMT, returns the parsed object tree."
  (let* ((text (cltpt/file-utils:read-file filepath))
         (text-tree (if text-object-types
                        (parse fmt text :text-object-types text-object-types)
                        (parse fmt text))))
    (when (typep text-tree 'document)
      (setf (document-src-file text-tree) filepath))
    text-tree))

(defgeneric text-format-escape (fmt text escapable-chars escape-newlines)
  (:documentation "handles special characters when converting across formats.

- FMT: destination format we are converting to,
- TEXT: string for escaping,
- ESCAPABLE-CHARS: an alist mapping a char to an escape sequence, see `cltpt/latex:*latex-escape-table*',
- ESCAPE-NEWLINES: whether to escape newlines too."))

;; default one
(defmethod text-format-escape (fmt
                               text
                               escapable-chars
                               escape-newlines)
  text)

(defgeneric text-format-conversion-template (fmt)
  (:documentation "a template for use when converting a document from some format into FMT."))

(defmethod text-format-conversion-template (fmt)
  nil)

(defmethod cltpt/base:text-format-conversion-template ((fmt (eql :simple)))
  "%(getf cltpt/base:*convert-info* :text-obj)")
