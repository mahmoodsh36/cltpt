(defpackage :cltpt/latex
  (:use :cl)
  (:export
   :*latex* :display-math
   :inline-math :display-math
   :latex-env :latex-link
   :*latex-code-env*
   :init))

(in-package :cltpt/latex)

(defvar *latex-template*
  "\\documentclass[11pt]{article}
\\usepackage{amsmath}
\\usepackage{hyperref}

\\author{mahmood sheikh}
\\title{%(cltpt/base:document-title (getf cltpt/base:*convert-info* :text-obj))}

\\begin{document}

%(getf cltpt/base:*convert-info* :text-obj)

\\end{document}"
  "a template for html conversion.")

(defvar *latex-code-env*
  "lstlisting")

(defun make-latex ()
  (cltpt/base:make-text-format
   "latex"
   '(display-math inline-math latex-env
     latex-link
     cltpt/base:text-macro cltpt/base:post-lexer-text-macro)))

(defun init ()
  )

(defvar *latex*
  (cltpt/base:make-text-format "latex")
  "`text-format' instance of the latex format.")

(defvar *latex-escape-table*
  `((#\& . "\\&")
    (#\~ . "\\textasciitilde{}")
    (#\\ . "\\textbackslash{}")
    (#\_ . "\\_")
    (#\# . "\\#")
    (#\newline . ,(format nil "~%\\\\~%"))))

(defmethod cltpt/base:text-format-escape ((fmt (eql *latex*))
                                          text
                                          escapable-chars
                                          escape-newlines)
  (if escape-newlines
      (cltpt/base:replace-chars-and-escapes
       ;; replace any sequence of newlines with a single newline
       ;; TODO: make into customizable behavior
       (cltpt/str-utils:compress-consec text #\newline)
       *latex-escape-table*
       escapable-chars)
      (cltpt/base:replace-chars-and-escapes
       text
       (remove #\newline *latex-escape-table* :key #'car)
       escapable-chars)))

(cltpt/base:define-text-object inline-math
  :rule '(:pattern
          (cltpt/combinator:pair
           (cltpt/combinator:unescaped (cltpt/combinator:literal "\\("))
           (cltpt/combinator:unescaped (cltpt/combinator:literal "\\)")))
          :on-char #\\))

(defmethod cltpt/base:text-object-init :after ((obj inline-math) str1 match)
  (setf (cltpt/base:text-object-property obj :is-inline)
        t))

(defmethod cltpt/base:text-object-convert ((obj inline-math)
                                           (fmt (eql *latex*)))
  (list :text (cltpt/base:text-object-text obj)
        :recurse t
        :escape nil))

(cltpt/base:define-text-object display-math
  :rule '(:pattern
          (cltpt/combinator:pair
           (cltpt/combinator:unescaped (cltpt/combinator:literal "\\["))
           (cltpt/combinator:unescaped (cltpt/combinator:literal "\\]")))
          :on-char #\\))

(defmethod cltpt/base:text-object-convert ((obj display-math)
                                           (fmt (eql *latex*)))
  (list :text (cltpt/base:text-object-text obj)
        :recurse t
        :escape nil))

(cltpt/base:define-text-object latex-env
  :rule `(:pattern
          (cltpt/combinator:pair
           (:pattern
            (cltpt/combinator:unescaped
             ,(cltpt/combinator:handle-rule-string "\\begin{%W}"))
            :id open-tag)
           (:pattern
            (cltpt/combinator:unescaped
             ,(cltpt/combinator:handle-rule-string "\\end{%W}"))
            :id close-tag))
          :on-char #\\)
  :documentation "latex environment.")

(cltpt/base:define-text-object latex-link
  :rule '(:pattern
          (cltpt/combinator:consec
           "\\ref{"
           (:pattern (cltpt/combinator::symbol-matcher)
            :id link-dest)
           "}")
          :on-char #\\)
  :documentation "latex link.")

(defmethod cltpt/base:text-object-convert ((obj latex-env) (fmt (eql *latex*)))
  (list :text (cltpt/base:text-object-text obj)
        :recurse nil
        :escape nil))

(defmethod cltpt/base:text-format-conversion-template ((fmt (eql *latex*)))
  *latex-template*)