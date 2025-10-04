(defpackage :cltpt/base
  (:use :cl)
  (:export
   :flatten :tree-find :pcase :plistp
   :bind-and-eval :bind-and-eval*
   :compress-consec :str-join :concat :str-prune :str-split
   :alist-get :subseq* :sorted-insert :last-atom
   :merge-plist :replace-substr
   :add-duration

   :region-incf

   :make-text-format
   :text-format-text-object-types :text-format-by-name
   :convert-file :text-macro :post-lexer-text-macro
   :text-format-escape :replace-chars-and-escapes
   :text-format

   :parse-file
   :parse :handle-changed-regions

   :convert-tree
   :*convert-escape-newlines*
   :*convert-info*
   :rewrap-within-tags

   :*author*
   :document :document-title :document-date
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
   :text-object-clone :sort-text-objects :text-object-set-parent
   :text-object-adjust-to-parent :text-object-extend-in-parent :text-object-move
   :text-block :text-object-convert-options

   :region-text))

(in-package :cltpt/base)