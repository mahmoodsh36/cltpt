(defpackage :cltpt/latex
  (:use :cl :cltpt/base :cltpt/combinator)
  (:shadowing-import-from :cltpt/combinator parse)
  (:export
   :latex :display-math
   :inline-math :*inline-math-rule*
   :latex-env
   :generate-latex-preamble :*latex-preamble*))

(in-package :cltpt/latex)

;; should be able to generate svg's (perhaps png's too) and have another 'mathjax option (atleast)
(defvar *html-export-with-latex-method*
  'svg)

(defvar *latex-preamble*
  "\\documentclass[11pt]{article}
\\usepackage{amsmath}")

(defun generate-latex-preamble (author date title)
  (format nil "~A
\\author{~A}
\\date{~A}
\\title{~A}
\\hypersetup{
 pdfauthor={~A},
 pdftitle={~A},
 pdfkeywords={},
 pdfsubject={},
 pdfcreator={cltpt},
 pdflang={English}}"
          *latex-preamble* author date title author title))

(defun make-latex ()
  (cltpt/base:make-text-format
   "latex"
   '(display-math inline-math latex-env
     latex-link
     cltpt/base:text-macro cltpt/base:post-lexer-text-macro)))
(defvar latex)
(eval-when (:load-toplevel :execute)
  (setf latex (make-latex)))

(defvar *latex-escape-table*
  '((#\& . "\\&")
    (#\~ . "\\textasciitilde{}")
    (#\\ . "\\textbackslash{}")
    (#\_ . "\\_")
    (#\# . "\\#")))

(defun latex-escape (s escapable-chars)
  s)

(defmethod cltpt/base:text-format-escape ((fmt (eql latex)) text escapable-chars)
  (cltpt/base:replace-chars-and-escapes text *latex-escape-table* escapable-chars))

(defun latex-fragment-to-html (latex-code is-inline)
  (case *html-export-with-latex-method*
    ('svg
     (let ((img-filepath))
       (let ((img-filepath (cdar (generate-svgs-for-latex (list latex-code)))))
         (format nil "<img src='~A'></img>" img-filepath))))))