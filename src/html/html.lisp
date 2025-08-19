(defpackage :cltpt/html
  (:use :cl :cltpt/base :cltpt/latex)
  (:export :*html* :generate-html-preamble :*html-static-route*
           :*html-postamble* :*html-preamble* :init))

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

(defvar *html-postamble*
  "</body>
</html>"
  "a template for html conversion.")

(defvar *html-preamble*
  "<!DOCTYPE html>
<html>
<head>
  <meta charset=\"UTF-8\">
  <title> %title </title>
</head>
  <body>"
  "a template for html conversion.")

(defmethod cltpt/base:text-format-generate-preamble ((fmt text-format)
                                                     (doc document))
  (cltpt/base:bind-and-eval
   `((title "mytitle")
     (author "myauthor")
     (date "mydate"))
   (lambda ()
     ;; need to use in-package to access the variables bound above
     (let ((*package* (find-package :cltpt/html))
           (result
             (cltpt/base:convert-tree
              (cltpt/base:parse
               *html-preamble*
               (list 'cltpt/base:text-macro 'cltpt/base:post-lexer-text-macro))
              *html*
              nil
              nil
              t
              nil)))
       result))))

(defmethod cltpt/base:text-format-generate-postamble ((fmt text-format) (doc document))
  (cltpt/base:bind-and-eval
   `((title "mytitle")
     (author "myauthor")
     (date "mydate"))
   (lambda ()
     ;; need to use in-package to access the variables bound above
     (let ((*package* (find-package :cltpt/html))
           (result
             (cltpt/base:convert-tree
              (cltpt/base:parse
               *html-postamble*
               (list 'cltpt/base:text-macro 'cltpt/base:post-lexer-text-macro))
              *html*
              nil)))
       result))))

(defvar *html-escape-table*
  '((#\newline . "<br>")
    (#\< . "&lt;")
    (#\< . "&gt;")
    (#\" . "&quot;")
    (#\' . "&apos;")
    (#\& . "&amp;")))

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