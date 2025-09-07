(defpackage :cltpt/html
  (:use :cl :cltpt/base :cltpt/latex)
  (:export :*html* :*html-static-route* :*html-static-dir*
           :*html-template* :init))

(in-package :cltpt/html)

(defvar *html*
  (make-text-format "html")
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
  (setf
   (cltpt/base:text-format-name *html*)
   "html"))

;; should be able to generate svg's (perhaps png's too) and have another 'mathjax option (atleast)
(defvar *html-export-latex-method*
  'svg)

(defvar *html-static-route*
  nil
  "the static path to which the links generated for html will point.

if nil, the paths will be absolute, otherwise, they will be join with the given
directory path.")

(defvar *html-static-dir*
  nil
  "the static path to which the static files for html will be copied.")

(defvar *html-template*
  "<!DOCTYPE html>
<html>
<head>
  <meta charset=\"UTF-8\">
  <title> %(cltpt/base:document-title (getf cltpt/base:*convert-info* :text-obj)) </title>
</head>
<body>
  #(cltpt/base::make-block :type 'div
                           :let* `((obj ,(getf cltpt/base:*convert-info* :text-obj))
                                   (contents ,(cltpt/base:text-object-contents obj))
                                   (date ,(cltpt/base:document-date obj))
                                   (title ,(cltpt/base:document-title obj))))
    <h1> %title - %date </h1>
    %contents
  #(cltpt/base::block-end)
</body>
</html>"
  "a template for html conversion.")

(defvar *html-escape-table*
  '((#\newline . "<br>")
    (#\< . "&lt;")
    (#\< . "&gt;")
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
       (cltpt/base:compress-consec text #\newline)
       *html-escape-table*
       escapable-chars)
      (cltpt/base:replace-chars-and-escapes
       text
       (remove #\newline *html-escape-table* :key #'car)
       escapable-chars)))