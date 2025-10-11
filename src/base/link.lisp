(in-package :cltpt/base)

;; this should be inherited from, it doesnt store the "destination" in a meaningful
;; way that can be resolved, just the destination as it was encountered in the text.
;; but we want the destination to be a `text-object' or a filepath.
(defstruct link
  "struct representing a, but doesnt define how it should be resolved."
  (src-text-obj nil "the text object which created the link (e.g. an instance of 'org-link')")
  desc
  dest)

(defgeneric link-create (src-text-obj link-type link-dest link-desc)
  (:documentation "return a `link' using the given properties."))

(defgeneric link-resolve (link-instance)
  (:documentation "given LINK-INSTANCE, return a destination filepath."))

;; we need an additional function for "exact" location resolving, filepath+position.
(defgeneric link-resolve-exact (link-instance)
  (:documentation "given LINK-INSTANCE, return a destination filepath+position."))

(defstruct (link-text-obj (:include link))
  "a `link' type that resolves to a text object."
  (filepath nil "the filepath in which the destination text object resides.")
  (dest-text-obj nil "the destination `text-object'."))

(defstruct (link-static (:include link))
  "a `link' type that resolves to a static file."
  (filepath nil "the destination filepath."))

(defmethod link-create ((obj text-object)
                        (link-type (eql 'file))
                        dest
                        desc)
  (make-link :src-text-obj obj
             :dest dest
             :desc desc))

;; for resolving static file links.
(defmethod link-resolve ((link-instance link-static))
  (link-filepath link-instance))