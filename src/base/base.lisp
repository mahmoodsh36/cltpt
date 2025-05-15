(defpackage :cltpt/base
  (:use :cl :str)
  (:export :parse :make-text-format
           :begin-of-line :end-of-line
           :text-format-text-object-types :text-format-by-name
           :convert-file :text-macro :text-macro-ref :post-lexer-text-macro
           :post-lexer-text-macro-ref :text-object
           :text-object-property :text-object-text
           :consec :literal :any :atleast-one :all-but :all-but-newline
           :text-object-init :region-begin :region-end :text-object-opening-region
           :text-object-convert :pcase :make-region :text-object-finalize
           :text-object-parent :text-object-next-sibling
           :literal-casein :region-text
           :text-object-prev-sibling :text-object-contents
           :symbol-matcher :org-list-parse
           :mapcar-forest :convert-tree
           :text-format-escape :replace-chars-and-escapes
           :document :map-text-object :wrap-contents-for-convert))

(in-package :cltpt/base)

(defvar *debug* nil)