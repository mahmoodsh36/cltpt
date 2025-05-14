(defpackage :cltpt/latex
  (:use :cl :cltpt/base)
  (:import-from :cltpt/base
   :text-object :text-format-escape
   :text-format :make-text-format
   :replace-chars-and-escapes)
  (:export :latex))

(in-package :cltpt/latex)

(defun make-latex ()
  (make-text-format
   "latex"
   '(display-math inline-math latex-env
     text-macro text-macro-ref
     post-lexer-text-macro post-lexer-text-macro-ref)))

(defvar latex)
(eval-when (:load-toplevel :execute)
  (setf latex (make-latex)))

(defvar *latex-escape-table*
  '((#\& . "\\&")
    (#\~ . "\\textasciitilde{}")
    (#\\ . "\\\\")
    (#\_ . "\\_")
    (#\# . "\\#")))

(defun latex-escape (s escapable-chars)
  )

(defmethod text-format-escape ((fmt (eql latex)) text escapable-chars)
  (replace-chars-and-escapes text *latex-escape-table* escapable-chars))