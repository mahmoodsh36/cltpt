(defpackage :cltpt/html
  (:use :cl)
  (:export
   :*html* :*html-static-route* :*html-template* :init :*html-export-latex-method*))

(in-package :cltpt/html)

(defvar *html*
  (cltpt/base:make-text-format "html")
  "the instance of `cltpt/base:text-object' for the html format.")

(defun init ()
  (setf
   (cltpt/base:text-format-text-object-types *html*)
   '(cltpt/latex:display-math
     cltpt/latex:inline-math
     cltpt/latex:latex-env
     ;; text-macro
     ;; post-lexer-text-macro
     ))
  (setf (cltpt/base:text-format-name *html*) "html"))

;; should be able to generate svg's (perhaps png's too) and have another 'mathjax option (atleast)
(defvar *html-export-latex-method*
  'svg)

(defvar *html-static-route*
  nil
  "the static path to which the links generated for html will point.

if nil, the paths will be absolute, otherwise, they will be join with the given
directory path.")

(defvar *html-template*
  "<!DOCTYPE html>
<html>
<head>
  <meta charset=\"UTF-8\">
  <title> %(cltpt/base:document-title (getf cltpt/base:*convert-info* :text-obj)) </title>
</head>
<body>
  <div class=\"post-content\">
    <h1> %(cltpt/base:document-title (getf cltpt/base:*convert-info* :text-obj)) - %(cltpt/base:document-date (getf cltpt/base:*convert-info* :text-obj)) </h1>
    %(getf cltpt/base:*convert-info* :text-obj)
  </div>
</body>
</html>"
  "a template for html conversion.")

(defvar *html-escape-table*
  `((#\newline . ,(format nil "~%<br>~%"))
    (#\< . "&lt;")
    (#\> . "&gt;")
    (#\" . "&quot;")
    (#\' . "&apos;")
    (#\& . "&amp;")
    ;; (#\space . "&nbsp;")
    ))

(defmethod cltpt/base:text-format-escape ((fmt (eql *html*))
                                          text
                                          escapable-chars
                                          escape-newlines)
  (if escape-newlines
      (cltpt/base:replace-chars-and-escapes
       ;; replace any sequence of newlines with a single newline
       ;; TODO: make into customizable behavior
       (cltpt/str-utils:compress-consec text #\newline)
       *html-escape-table*
       escapable-chars)
      (cltpt/base:replace-chars-and-escapes
       text
       (remove #\newline *html-escape-table* :key #'car)
       escapable-chars)))

(defmethod cltpt/base:text-format-conversion-template ((fmt (eql *html*)))
  *html-template*)