(in-package :cltpt/org-mode)

;; should be able to generate svg's (perhaps png's too) and have another 'mathjax option (atleast)
(defvar *html-export-with-latex-method*
  'svg)

(defvar *html-escape-table*
  )

(defun generate-html-header (author date title)
  "<head></head>")

;; A
(defun org-list-to-html (org-forest)
  (org-list-to-html-list org-forest))
(defun org-list-to-html-item (cons-item)
  (let* ((node (car cons-item))
         (text (getf node :text))
         (children (cdr cons-item)))
    (format nil "<li>~A~A</li>"
            text
            (if children (org-list-to-html-list children) ""))))
(defun org-list-to-html-list (forest)
  (format nil "<ul>~{~A~}</ul>" (mapcar #'org-list-to-html-item forest)))

(defun org-table-to-html (table)
  "generate an html table from TABLE.
TABLE is a list of rows (each row is a list of strings)."
  (with-output-to-string (out)
    (format out "<table border='1'>~%")
    (dolist (row table)
      (format out "  <tr>~%")
      (dolist (cell row)
        (format out "    <td>~a</td>~%" cell))
      (format out "  </tr>~%"))
    (format out "</table>")))

(defun latex-fragment-to-html (latex-code is-inline)
  (case *html-export-with-latex-method*
    ('svg
     (let ((img-filepath))
       (let ((img-filepath (cdar (generate-svgs-for-latex (list latex-code)))))
         (format nil "<img src='~A'></img>" img-filepath))))))