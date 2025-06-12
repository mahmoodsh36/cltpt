(defpackage :cltpt/base
  (:use :cl :str)
  (:export
   :parse :make-text-format
   :begin-of-line :end-of-line
   :text-format-text-object-types :text-format-by-name
   :convert-file :text-macro :post-lexer-text-macro :text-object
   :text-object-property :text-object-text
   :text-object-init :region-begin
   :region-end :text-object-convert
   :pcase :make-region :text-object-finalize :text-object-parent :text-object-next-sibling
   :mapcar-forest :convert-tree :text-format-escape :replace-chars-and-escapes
   :wrap-contents-for-convert
   :document :map-text-object
   :region-text :text-object-prev-sibling :text-object-contents))

(in-package :cltpt/base)

(defvar *debug* nil)