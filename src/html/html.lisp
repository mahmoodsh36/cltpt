(defpackage :cltpt/html
  (:use :cl :cltpt/base :cltpt/latex)
  (:export :html))

(in-package :cltpt/html)

(defun make-html ()
  (make-text-format
   "html"
   '(display-math inline-math latex-env
     text-macro post-lexer-text-macro)))

(defvar html (make-html))

;; should be able to generate svg's (perhaps png's too) and have another 'mathjax option (atleast)
(defvar *html-export-latex-method*
  'svg)

;; (defmethod text-format-escape ((fmt (eql html)) text escapable-chars)
;;   text)