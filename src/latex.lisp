(in-package :cltpt)

(defun make-latex ()
  (make-text-format
   "latex"
   '(display-math inline-math latex-env
     text-macro text-macro-ref
     post-lexer-text-macro post-lexer-text-macro-ref)))

(defvar latex (make-latex))