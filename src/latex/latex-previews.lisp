(in-package :cltpt/latex)

(defparameter *latex-previews-tmp-directory* #P"/tmp/")
(defparameter *latex-previews-preamble-filename* "preamble") ;; appended with .fmt
(defparameter *latex-command* "latex")
(defparameter *dvisvgm-command* "dvisvgm")
(defparameter *preview-filename-prefix* "cltpt-snippet-")

(defun ensure-cached-format (format-path)
  "ensure that the cached format file exists at FORMAT-PATH.
if it doesnt exist, generate it by dumping the preamble.
uses *latex-command* and *latex-preamble*."
  (unless (probe-file format-path)
    (let ((preamble-file (merge-pathnames "preamble.tex" *latex-previews-tmp-directory*)))
      (with-open-file (out preamble-file
                           :direction :output
                           :if-exists :supersede)
        (format out "~A
\\usepackage[active,tightpage,auctex,dvips]{preview}
\\dump~%"
                *latex-preamble*))
      (multiple-value-bind (out-str err-str exit-code)
          (uiop:run-program
           (list *latex-command*
                 "-ini"
                 (format nil "-jobname=~A" *latex-previews-preamble-filename*)
                 (format nil "&~A" *latex-command*)
                 (namestring preamble-file))
           :output t
           :directory *latex-previews-tmp-directory*
           :wait t))
      (delete-file preamble-file))))

(defun cleanup-temp-files (base-name)
  "delete temporary files associated with BASE-NAME in *latex-previews-tmp-directory*."
  (dolist (ext '(".tex" ".dvi" ".aux" ".log"))
    (let ((path (merge-pathnames (concatenate 'string base-name ext) *latex-previews-tmp-directory*)))
      (when (probe-file path)
        (delete-file path)))))

(defun generate-svgs-for-latex (snippets &optional (recompile nil))
  "generate SVG files for a list of latex SNIPPETS using a cached preamble.
returns an association list of (hash . svg-file-path). if the SVG file for a snippet already exists (with the correct hash),
this function does nothing for that snippet."
  (unless snippets
    (return-from generate-svgs-for-latex nil))
  (ensure-directory *latex-previews-tmp-directory*)
  (let* ((snippet-hashes (mapcar #'md5-str snippets))
         (expected-svgs (mapcar
                         (lambda (hash)
                           (merge-pathnames
                            (concatenate 'string
                                         *preview-filename-prefix*
                                         hash
                                         ".svg")
                            *latex-previews-tmp-directory*))
                         snippet-hashes)))
    ;; if all expected SVG files exist (and we're not forcing a recompile), return them.
    (when (and (not recompile)
               (every #'probe-file expected-svgs))
      (return-from generate-svgs-for-latex
        (mapcar
         (lambda (hash)
           (cons hash
                 (merge-pathnames
                  (concatenate 'string
                               *preview-filename-prefix*
                               hash
                               ".svg")
                  *latex-previews-tmp-directory*)))
         snippet-hashes)))
    (let* ((combined-hash (md5-str (apply #'concatenate 'string snippet-hashes)))
           (multi-page-base (concatenate 'string *preview-filename-prefix* combined-hash))
           ;; build the intermediate SVG naming pattern.
           ;; here we use "-%9p.svg" so dvisvgm substitutes a 9-digit, zero-padded page number.
           (svg-name-pattern (concatenate 'string
                                          (namestring *latex-previews-tmp-directory*)
                                          multi-page-base
                                          "-%9p.svg"))
           (fmt-path (merge-pathnames (concatenate 'string
                                                   *latex-previews-preamble-filename* ".fmt")
                                      *latex-previews-tmp-directory*))
           (tex-file (merge-pathnames (concatenate 'string multi-page-base ".tex")
                                      *latex-previews-tmp-directory*))
           (dvi-file (merge-pathnames (concatenate 'string multi-page-base ".dvi")
                                      *latex-previews-tmp-directory*)))
      ;; clean up any previous temporary files with the same base.
      (dolist (ext '(".tex" ".dvi" ".aux" ".log"))
        (let ((path (merge-pathnames (concatenate 'string multi-page-base ext)
                                     *latex-previews-tmp-directory*)))
          (when (probe-file path)
            (delete-file path))))
      (when recompile
        (uiop:delete-file-if-exists fmt-path))
      (ensure-cached-format fmt-path)
      ;; write the multi-page tex file.
      (with-open-file (out tex-file :direction :output :if-exists :supersede)
        (format out "\\documentclass{standalone}~%")
        (format out "\\begin{document}~%")
        (dolist (snippet snippets)
          (format out "\\begin{preview}~%")
          (format out "~A~%" snippet)
          (format out "\\end{preview}\\newpage~%"))
        (format out "\\end{document}~%"))
      ;; compile the tex file to produce a dvi.
      (multiple-value-bind (latex-out latex-err latex-exit)
          (uiop:run-program
           (list *latex-command*
                 (format nil "-fmt=~A" (namestring fmt-path))
                 "-interaction=nonstopmode"
                 "-output-format=dvi"
                 (format nil
                         "-output-directory=~A"
                         (namestring *latex-previews-tmp-directory*))
                 (namestring tex-file))
           :output t
           :ignore-error-status t
           :wait t))
      ;; convert the dvi file to svg(s) using dvisvgm.
      (multiple-value-bind (dvisvgm-out dvisvgm-err dvisvgm-exit)
          (uiop:run-program
           (list *dvisvgm-command*
                 (namestring dvi-file)
                 "--no-fonts"
                 "--page=1-"
                 "--clipjoin"
                 "--optimize"
                 "--bbox=preview"
                 "-o" svg-name-pattern)
           :output t
           :wait t))
      ;; rename each generated SVG file to include the corresponding snippet hash.
      (let ((result
              (loop
                for i from 1 to (length snippets)
                for hash in snippet-hashes
                for old-svg = (merge-pathnames (format nil
                                                       "~A-~9,'0d.svg"
                                                       multi-page-base
                                                       i)
                                               *latex-previews-tmp-directory*)
                for new-svg = (merge-pathnames (concatenate 'string
                                                            *preview-filename-prefix*
                                                            hash
                                                            ".svg")
                                               *latex-previews-tmp-directory*)
                do (when (probe-file old-svg)
                     (rename-file old-svg new-svg))
                finally (return
                          (mapcar
                           (lambda (hash)
                             (cons
                              hash
                              (merge-pathnames
                               (concatenate 'string
                                            *preview-filename-prefix*
                                            hash
                                            ".svg")
                               *latex-previews-tmp-directory*)))
                           snippet-hashes)))))
        result))))