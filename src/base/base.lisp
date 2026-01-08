(defpackage :cltpt/base
  (:use :cl)
  (:export
   :tree-find :pcase :plistp
   :bind-and-eval :bind-and-eval* :eval-in-text-object-lexical-scope
   :concat :alist-get :subseq* :sorted-insert :last-atom
   :merge-plist :add-duration :list-dates :list-date-pairs
   :today-timestamp :truncate-to-day
   :find-cons-if :tautology

   :make-text-format :*simple-format*
   :text-format-text-object-types :text-format-by-name
   :convert-file :text-macro :post-lexer-text-macro
   :text-format-escape :replace-chars-and-escapes
   :text-format :text-format-conversion-template

   :parse-file :parse
   :reparse-change-in-tree :make-reparse-callback

   :convert-tree :convert-document
   :*convert-escape-newlines*
   :*convert-info*
   :rewrap-within-tags
   :convert-simple-format
   :filepath-format

   :*author*
   :text-object
   :text-object-convert :text-object-children
   :map-text-object :map-text-object-with-pos-in-root
   :text-object-begin :text-object-end
   :text-object-finalize :text-object-parent :text-object-next-sibling
   :text-object-prev-sibling :text-object-contents
   :text-object-property :text-object-text :text-object-text-region
   :text-object-properties
   :text-object-init
   :text-object-begin-in-root :text-object-end-in-root
   :find-children-recursively :find-children :list-children-recursively :find-ancestor
   :child-at-pos
   :text-object-rule-from-subclass
   :text-format-name :text-format-from-alias :text-format-document-type
   :text-object-clone :sort-text-objects :text-object-set-parent
   :text-object-adjust-to-parent :text-object-extend-in-parent :text-object-move
   :text-block :text-object-convert-options :text-object-contents-region
   :text-object-match
   :text-link :text-link-link :text-link-resolve
   :link-resolve :link-desc :link-dest :link-type
   :target-filepath :target-text-object :convert-target-filepath
   :make-block :block-end
   :text-object-match-text :text-object-root
   :text-object-force-set-text
   :text-object-find-submatch
   :text-object-text-length
   :is-post-lexer-macro :post-lexer-macro-value
   :eval-post-lexer-macro

   :document :document-title :document-date
   :document-src-file

   :*image-ext* :*video-ext*))

(in-package :cltpt/base)

(defvar *image-ext*
  (list "png" "webp" "svg" "jpg" "jpeg" "gif")
  "a list holding the file extensions that should be recognized as image links.")

(defvar *video-ext*
  (list "mp4")
  "a list holding the file extensions that should be recognized as video links.")

(defvar cl-user::*file-info*
  nil
  "this is dynamically bound during conversion and used in `filepath-format-info' to handle the format string for static files.")