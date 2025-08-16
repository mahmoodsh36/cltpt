(defpackage :cltpt/latex
  (:use :cl :cltpt/base :cltpt/combinator)
  (:shadowing-import-from :cltpt/combinator parse)
  (:export
   :*latex* :display-math
   :inline-math :*inline-math-rule* :display-math :*display-math-rule*
   :latex-env :*latex-env-rule*
   :generate-latex-preamble :*latex-preamble* :*latex-preview-preamble*
   :*latex-previews-cache-directory* :generate-svgs-for-latex))

(in-package :cltpt/latex)

(defvar *latex-preamble*
  "\\documentclass[11pt]{article}
\\usepackage{amsmath}
\\usepackage{hyperref}")

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

(defvar *latex*
  (make-latex)
  "`text-format' instance of the latex format.")

(defvar *latex-escape-table*
  '((#\& . "\\&")
    (#\~ . "\\textasciitilde{}")
    (#\\ . "\\textbackslash{}")
    (#\_ . "\\_")
    (#\# . "\\#")
    (#\newline . "\\\\")))

(defmethod cltpt/base:text-format-escape ((fmt (eql *latex*))
                                          text
                                          escapable-chars
                                          escape-newlines)
  (if escape-newlines
      (cltpt/base:replace-chars-and-escapes
       ;; replace any sequence of newlines with a single newline
       ;; TODO: make into customizable behavior
       (cltpt/base:compress-consec text #\newline)
       *latex-escape-table*
       escapable-chars)
      (cltpt/base:replace-chars-and-escapes
       text
       (remove #\newline *latex-escape-table* :key #'car)
       escapable-chars)))