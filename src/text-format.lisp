(in-package :cltpt)

;; holds all text formats
(defvar *text-formats* nil)

(defclass text-format ()
  ((name
    :initarg :name
    :accessor text-format-name
    :documentation "the unique name of the text format, it is used to identify the format from the commandline (among other places).")
   (text-object-types
    :initarg :text-object-types
    :accessor text-format-text-object-types
    :documentation "the `text-object' subclasses that the format corresponds to.")
   (text-document-type
    :initarg :text-document-type
    :accessor text-format-text-document-type
    :documentation "the `text-document' (sub)class that corresponds to this format.")))

(defgeneric text-format-convert (src-text-format dest-text-format text)
  (:documentation "convert from one text format into another."))

(defun make-text-format (name text-object-types &optional (doc-type 'document))
  (let ((fmt (make-instance 'text-format))
        (existent (text-format-by-name name)))
    (when existent
      (setf *text-formats* (delete existent *text-formats*)))
    (push fmt *text-formats*)
    (setf (text-format-name fmt) name)
    (setf (text-format-text-object-types fmt) text-object-types)
    (setf (text-format-text-document-type fmt) doc-type)
    fmt))

(defmethod text-format-convert ((fmt1 text-format) (fmt2 text-format) text)
  (convert-text fmt1 fmt2 text))

(defun convert-file (fmt1 fmt2 src-file dest-file)
  (let* ((text (uiop:read-file-string src-file))
         (result (convert-text fmt1 fmt2 text)))
    (with-open-file (f dest-file
                     :direction :output
                     :if-exists :supersede
                     :if-does-not-exist :create)
    (write-sequence result f))))

(defun convert-text (fmt1 fmt2 text)
  (let* ((text-tree (parse text
                           (text-format-text-object-types fmt1)
                           :doc-type (text-format-text-document-type fmt1)))
         (result (convert-tree text-tree
                               fmt2
                               (text-format-text-object-types fmt1))))
    result))

(defun text-format-by-name (name)
  (find name *text-formats* :key 'text-format-name :test 'string=))