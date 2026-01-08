(in-package :cltpt/base)

;; "struct representing a link, but doesnt define how it should be resolved."
(defstruct link
  src
  type
  desc
  dest)

(defgeneric link-resolve (src type dest desc)
  (:documentation "given a link (by its properties), return a target.

target can be a filepath or a text-object. perhaps this isnt the best way
to handle this but it should work. we should define exactly what a target is
in the future."))

(defgeneric target-filepath (target)
  (:documentation "given a target that was returned by `link-resolve', return the filepath associated with it (if any)."))

(defgeneric convert-target-filepath (target)
  (:documentation "given a target that was returned by `link-resolve', return the filepath associated with it (if any).

this function should always return a relative path that will be appended to :dest-dir during conversion."))

(defmethod target-filepath ((target string))
  target)

(defmethod target-filepath ((target pathname))
  (cltpt/file-utils:ensure-filepath-string target))

(defmethod convert-target-filepath ((target t))
  (target-filepath target))

(defmethod link-resolve (src
                         (link-type (eql 'file))
                         dest
                         desc)
  (uiop:parse-unix-namestring dest))

;; default to 'file functionality
(defmethod link-resolve (src
                         (link-type symbol)
                         dest
                         desc)
  (link-resolve src 'file dest desc))

(defgeneric target-text-object (target)
  (:documentation "given a target that was returned by `link-resolve', return the text-object associated with it (if any)."))

;; when requesting a text-object for a filepath we parse the file and return the document.
(defmethod target-text-object ((target t))
  nil)

;; this currently just adapts the default org-attach id for org-id's
;; d0bddd39-2180-4e8d-895b-f64bcb6472ea -> ./data/d0/bddd39-2180-4e8d-895b-f64bcb6472ea
(defun id-to-attach-dir (src-file id)
  (cltpt/file-utils:join-paths
   (cltpt/file-utils:file-dirpath src-file)
   "data"
   (subseq id 0 2)
   (subseq id 2)))

(defvar *id-to-attach-dir-func*
  'id-to-attach-dir
  "the function to call for converting an id and src-file to a destination file. for org-attach-like functionality.")