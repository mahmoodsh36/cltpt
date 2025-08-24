(defpackage :cltpt/latex
  (:use :cl :cltpt/base :cltpt/combinator)
  (:shadowing-import-from :cltpt/combinator parse)
  (:export
   :*latex* :display-math
   :inline-math :*inline-math-rule* :display-math :*display-math-rule*
   :latex-env :*latex-env-rule*
   :generate-latex-preamble :*latex-preamble* :*latex-preview-preamble*
   :*latex-previews-cache-directory* :generate-svgs-for-latex :init))

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

(defun init ()
  )

(defvar *latex*
  (make-text-format "latex")
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

(defvar *inline-math-rule*
  '(:pattern
    (cltpt/combinator:pair
     (cltpt/combinator:unescaped (cltpt/combinator:literal "\\("))
     (cltpt/combinator:unescaped (cltpt/combinator:literal "\\)")))
    :on-char #\\))
(defclass inline-math (cltpt/base:text-object)
  ((cltpt/base::rule
    :allocation :class
    :initform *inline-math-rule*)))

(defmethod cltpt/base:text-object-convert ((obj inline-math) (fmt (eql *latex*)))
  (list :text (cltpt/base:text-object-text obj)
        :recurse t
        :escape nil))

(defvar *display-math-rule*
  '(:pattern
    (cltpt/combinator:pair
     (cltpt/combinator:unescaped (cltpt/combinator:literal "\\["))
     (cltpt/combinator:unescaped (cltpt/combinator:literal "\\]")))
    :on-char #\\))
(defclass display-math (cltpt/base:text-object)
  ((cltpt/base::rule
    :allocation :class
    :initform *display-math-rule*)))

(defmethod cltpt/base:text-object-convert ((obj display-math)
                                           (fmt (eql *latex*)))
  (list :text (cltpt/base:text-object-text obj)
        :reparse nil
        :recurse t
        :escape nil))

(defvar *latex-env-rule*
  `(:pattern
    (cltpt/combinator:pair
     (cltpt/combinator:unescaped ,(cltpt/combinator:handle-rule-string "\\begin{%W}"))
     (cltpt/combinator:unescaped ,(cltpt/combinator:handle-rule-string "\\end{%W}")))
    :on-char #\\))
(defclass latex-env (cltpt/base:text-object)
  ((cltpt/base::rule
    :allocation :class
    :initform *latex-env-rule*))
  (:documentation "latex environment."))

(defclass latex-link (cltpt/base:text-object)
  ((cltpt/base::shared-name
    :allocation :class
    :initform 'cltpt/base::link)
   (cltpt/base::rule
    :allocation :class
    :initform '(:pattern
                (cltpt/combinator:consec
                 "\\ref{"
                 (:pattern (cltpt/combinator::symbol-matcher)
                  :id link-dest)
                 "}")
                :on-char #\\)))
  (:documentation "latex link."))

(defmethod cltpt/base:text-object-convert ((obj latex-env) (fmt (eql *latex*)))
  (list :text (cltpt/base:text-object-text obj)
        :reparse nil
        :recurse nil
        :escape nil))