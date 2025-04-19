(in-package :cltpt)

(defun make-html ()
  (make-text-format
   "html"
   '(display-math inline-math latex-env
     text-macro text-macro-ref
     post-lexer-text-macro post-lexer-text-macro-ref)))

(defvar html (make-html))