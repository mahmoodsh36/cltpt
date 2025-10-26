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

;; a `link' type that resolves to a text object.
;; (defstruct (link-text-obj (:include link))
;;   ;; the filepath in which the destination text object resides.
;;   ;; the destination `text-object'.
;;   dest-text-obj)

(defmethod link-resolve ((link-type (eql 'cltpt/base::file))
                         dest
                         desc)
  (pathname dest))

(defmethod link-resolve ((link-type symbol)
                         dest
                         desc)
  nil)

(defmethod link-resolve ((link-type (eql 'cltpt/base::id))
                         dest
                         desc)
  ;; should return a 'target' that includes a filepath and a position in the file
  )

;; (defmethod text-object-link ((text-obj text-object))
;;   (let* ((link-type (cltpt/base:text-object-property obj :type))
;;          (link-desc (cltpt/base:text-object-property obj :dest))
;;          (link-dest (cltpt/base:text-object-property obj :desc)))
;;     (when link-dest
;;       (let ((l (link-create text-obj)))
;;         (link-create text-obj
;;                      (when link-type
;;                        (intern link-type))
;;                      link-dest
;;                      link-desc)))))