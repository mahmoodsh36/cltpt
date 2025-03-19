(in-package :cltpt)

(defvar *latex-preamble*
  "\\documentclass[11pt]{article}
\\usepackage{\\string~/.emacs.d/common}")

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

(defun latex-escape-chars (s &optional (escape-table '((#\& . "\\&")
                                                       (#\~ . "\\textasciitilde{}")
                                                       (#\\ . "\\\\")
                                                       (#\_ . "\\_")
                                                       (#\# . "\\#"))))
  "return a new string where every character in S that is a key in ESCAPE-TABLE is replaced by its associated string.
ESCAPE-TABLE is an association list mapping characters to replacement strings."
  (with-output-to-string (out)
    (loop for ch across s do
      (let ((replacement (cdr (assoc ch escape-table :test #'char=))))
        (if replacement
            (write-string replacement out)
            (write-char ch out))))))

;; A
(defun org-list-to-latex (org-forest)
  (org-list-to-latex-list org-forest))
(defun org-list-to-latex-item (cons-item)
  (let* ((node (car cons-item))
         (marker (getf node :marker))
         (text (getf node :text))
         (children (cdr cons-item)))
    (format nil "\\item ~A~A"
            text
            (if children (org-list-to-latex-list children) ""))))
(defun org-list-to-latex-list (forest)
  (let ((env (if (every (lambda (cons-item)
                          (let ((node (car cons-item)))
                            (cl-ppcre:scan "^[a-zA-Z]+\\.$" (getf node :marker))))
                        forest)
                 "enumerate"
                 "itemize")))
    (format nil "\\begin{~A}~%~{~A~%~}\\end{~A}" env (mapcar #'org-list-to-latex-item forest) env)))

;; A
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

;; A
(defun compile-latex-file (tex-file)
  "compile a LaTeX file using pdflatex under /tmp.
removes any leftover compilation files from previous runs.
TEX-FILE is the path to the .tex file.
returns the path to the produced PDF as a string, or NIL if compilation fails."
  (let* ((tex-path (pathname tex-file))
         (base (pathname-name tex-path))
         (tmp-dir #p"/tmp/")
         (tmp-tex (merge-pathnames (concatenate 'string base ".tex") tmp-dir))
         (tmp-pdf (merge-pathnames (concatenate 'string base ".pdf") tmp-dir))
         (tmp-aux (merge-pathnames (concatenate 'string base ".aux") tmp-dir))
         (tmp-log (merge-pathnames (concatenate 'string base ".log") tmp-dir)))
    ;; remove leftover files if they exist.
    (dolist (file (list tmp-tex tmp-pdf tmp-aux tmp-log))
      (when (probe-file file)
        (delete-file file)))
    ;; copy the source .tex file to /tmp.
    (uiop:copy-file tex-file tmp-tex)
    ;; run pdflatex in /tmp.
    (let ((result (uiop:run-program
                   (list "pdflatex" "-interaction=nonstopmode" (namestring tmp-tex))
                   :directory (namestring tmp-dir)
                   :output *standard-output*)))
      (unless (zerop (slot-value result 'exit-code))
        (return-from compile-latex-file nil)))
    ;; return the PDF path if it was produced; otherwise return NIL.
    (if (probe-file tmp-pdf)
        (namestring tmp-pdf)
        nil)))