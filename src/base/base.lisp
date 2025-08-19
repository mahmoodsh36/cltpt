(defpackage :cltpt/base
  (:use :cl :str)
  (:export
   :parse :make-text-format
   :text-format-text-object-types :text-format-by-name
   :convert-file :text-macro :post-lexer-text-macro
   :text-format-escape :replace-chars-and-escapes
   :text-format
   :parse-file
   :flatten
   :tree-find
   :pcase :plistp
   :bind-and-eval :bind-and-eval*
   :compress-consec

   :change-extension :path-without-extension :base-name-no-ext :change-dir
   :file-has-extension-p :file-ext :file-basename
   :ensure-directory

   :convert-tree

   :document
   :text-object
   :region-end :region-begin :region-length
   :text-object-convert :text-object-children
   :map-text-object :map-text-object-with-pos-in-root
   :text-object-begin :text-object-end
   :text-object-finalize :text-object-parent :text-object-next-sibling
   :text-object-prev-sibling :text-object-contents
   :text-object-property :text-object-text :text-object-text-region
   :text-object-properties
   :text-object-init
   :text-object-begin-in-root :text-object-end-in-root
   :make-region
   :wrap-contents-for-convert
   :find-children-recursively :find-children :list-children-recursively
   :child-at-pos :region-contains
   :text-object-rule-from-subclass
   :text-format-name :text-format-from-alias :text-format-document-type
   :text-format-generate-preamble :text-format-generate-postamble
   :text-object-clone

   :region-text))

(in-package :cltpt/base)

(defvar *debug* nil)