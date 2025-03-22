(in-package :cltpt)

(defparameter *latex-previews-tmp-directory* #P"/tmp/")
(defparameter *latex-command* "latex")
(defparameter *dvisvgm-command* "dvisvgm")

(defun ensure-cached-format (format-path)
  "ensure that the cached format file exists at FORMAT-PATH.
if it doesnt exist, generate it by dumping the preamble.
uses *latex-command* and *latex-preamble*."
  (unless (probe-file format-path)
    (let ((preamble-file (merge-pathnames "preamble.tex" *latex-previews-tmp-directory*)))
      (with-open-file (out preamble-file
                           :direction :output
                           :if-exists :supersede)
        (format out "~A~%\\dump~%" *latex-preamble*))
      (multiple-value-bind (out-str err-str exit-code)
          (uiop:run-program
           (list *latex-command* "-ini" "-jobname=standalone"
                 (format nil "&~A" *latex-command*)
                 (namestring preamble-file))
           :output t
           :directory *latex-previews-tmp-directory*
           :wait t)
        (unless (and exit-code (zerop exit-code))
          (error "latex failed to compile the cached preamble. exit code: ~A. error: ~A"
                 exit-code err-str)))
      (delete-file preamble-file)
      (format t "cached format generated at ~A~%" (namestring format-path)))))

(defun cleanup-temp-files (base-name)
  "delete temporary files associated with BASE-NAME in *latex-previews-tmp-directory*."
  (dolist (ext '(".tex" ".dvi" ".aux" ".log"))
    (let ((path (merge-pathnames (concatenate 'string base-name ext) *latex-previews-tmp-directory*)))
      (when (probe-file path)
        (delete-file path)))))

(defun generate-svg-for-latex (snippet &optional (recompile nil))
  "generate an SVG file for the latex snippet SNIPPET using cached preamble compilation.
the snippet is hashed (using MD5) to produce a unique filename.
if a snippet with the same contents was generated before, its SVG file is reused."
  (ensure-directory *latex-previews-tmp-directory*)
  (let* ((fmt-path (merge-pathnames "standalone.fmt" *latex-previews-tmp-directory*)))
    (when recompile
      (uiop:delete-file-if-exists fmt-path))
    (ensure-cached-format fmt-path)
    (let* ((hash (md5-str snippet))
           (base-name (format nil "snippet-~A" hash))
           (tex-file (merge-pathnames (concatenate 'string base-name ".tex")
                                      *latex-previews-tmp-directory*))
           (dvi-file (merge-pathnames (concatenate 'string base-name ".dvi")
                                      *latex-previews-tmp-directory*))
           (svg-file (merge-pathnames (concatenate 'string base-name ".svg")
                                      *latex-previews-tmp-directory*)))
      (cleanup-temp-files base-name)
      (unless (probe-file svg-file)
        ;; write the snippet file with proper document structure.
        (with-open-file (out tex-file
                             :direction :output
                             :if-exists :supersede)
          (format out "\\begin{document}~%")
          (format out "~A~%" snippet)
          (format out "\\end{document}~%"))
        ;; run latex using the cached format, use dvi output.
        (multiple-value-bind (out-str err-str exit-code)
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
             :wait t)
          (unless (and exit-code (zerop exit-code))
            (error "latex failed to compile ~A. exit code: ~A. error: ~A"
                   (namestring tex-file) exit-code err-str)))
        ;; convert the resulting dvi file to an svg.
        (multiple-value-bind (out-str err-str exit-code)
            (uiop:run-program
             (list *dvisvgm-command*
                   (namestring dvi-file)
                   "-n" ;; convert fonts to paths
                   "-o" (namestring svg-file))
             :output t
             :wait t)
          (unless (and exit-code (zerop exit-code))
            (error "dvisvgm failed to convert ~A. exit code: ~A. error: ~A"
                   (namestring dvi-file) exit-code err-str)))
        (format t "generated ~A~%" (namestring svg-file)))
      svg-file)))