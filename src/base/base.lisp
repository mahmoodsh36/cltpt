(defpackage :cltpt/base
  (:use :cl :str)
  (:export
   :parse :make-text-format
   :text-format-text-object-types :text-format-by-name
   :convert-file :text-macro :post-lexer-text-macro
   :pcase :make-region
   :convert-tree :text-format-escape :replace-chars-and-escapes
   :wrap-contents-for-convert
   :text-format

   :document
   :text-object
   :region-end :region-begin :region-length
   :text-object-convert :text-object-children
   :map-text-object
   :text-object-begin :text-object-end
   :text-object-finalize :text-object-parent :text-object-next-sibling
   :text-object-prev-sibling :text-object-contents
   :text-object-property :text-object-text :text-object-text-region
   :text-object-init

   :find-submatch
   :tree-find
   :region-text))

(in-package :cltpt/base)

(defvar *debug* nil)