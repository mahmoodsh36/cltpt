(in-package :cltpt/org-mode)

(defun org-list-to-latex (org-forest)
  (org-list-to-latex-list org-forest))
(defun org-list-to-latex-item (item)
  (let* ((node (car item))
         (marker (getf node :marker))
         (text (getf node :text))
         (children (cdr item)))
    (format nil "\\item ~A~A"
            text
            (if children (org-list-to-latex-list children) ""))))
(defun org-list-to-latex-list (forest)
  (let ((env (if (every (lambda (item)
                          (let ((node (car item)))
                            (cl-ppcre:scan "^[a-zA-Z]+\\.$" (getf node :marker))))
                        forest)
                 "enumerate"
                 "itemize")))
    (format nil "\\begin{~A}~%~{~A~%~}\\end{~A}" env (mapcar #'org-list-to-latex-item forest) env)))

(defun org-table-to-latex (table)
  "generate a LaTeX table from TABLE.
TABLE is a list of rows (each row is a list of strings)."
  (let* ((num-cols (length (first table)))
         ;; build the column spec: for 3 columns, this produces "|c|c|c|"
         (colspec (with-output-to-string (s)
                    (format s "|")
                    (dotimes (i num-cols)
                      (format s "c|")))))
    (with-output-to-string (out)
      (format out "\\begin{tabular}{~a}~%" colspec)
      (format out "\\hline~%")
      ;; loop over each row; join the cells with "&" and end with "\\\\ \\hline"
      (dolist (row table)
        (format out "~{~a~^ & ~} \\\\ \\hline~%" row))
      (format out "\\end{tabular}"))))