(in-package :cltpt/latex)

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