(in-package :cltpt/base)

;; "struct representing a link, but doesnt define how it should be resolved."
(defstruct link
  type
  desc
  dest)

(defgeneric link-resolve (type desc dest)
  (:documentation "given a link (by its properties), return a target.

target can be a filepath or a text-object. perhaps this isnt the best way
to handle this but it should work. we should define exactly what a target is
in the future."))

(defgeneric target-filepath (target)
  (:documentation "given a target that was returned by `link-resolve', return the filepath associated with it (if any)."))

(defmethod target-filepath ((target string))
  target)

(defmethod target-filepath ((target pathname))
  (cltpt/file-utils:ensure-filepath-string resolved))

(defmethod link-resolve ((link-type (eql 'file))
                         dest
                         desc)
  (uiop:parse-unix-namestring dest))

(defmethod link-resolve ((link-type symbol)
                         dest
                         desc)
  nil)