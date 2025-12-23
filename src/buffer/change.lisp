(defpackage :cltpt/buffer/change
  (:use :cl :cltpt/buffer/region)
  (:export
   :make-change :change-region :change-operator :change-args :change-begin :change-end))

(in-package :cltpt/buffer/change)

(defstruct change
  region
  ;; operator could be a string to replace the region with, or a function to pass the region's text to and replace it with the result.
  operator
  ;; args is a plist, could contain: discard-contained, delegate, new-level
  args)

(defmethod change-begin ((change change))
  (region-begin (change-region change)))

(defmethod change-end ((change change))
  (region-end (change-region change)))