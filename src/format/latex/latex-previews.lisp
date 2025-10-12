(in-package :cltpt/latex)

;; TODO: turn this into an independent submodule

(defun get-cltpt-subdirectory (name)
  "creates and returns a subdirectory within the system's temporary directory."
  (let ((path (uiop:ensure-directory-pathname
               (uiop:merge-pathnames*
                (make-pathname
                 :directory (list :relative "cltpt-latex-previews" name))
                (uiop:temporary-directory)))))
    (ensure-directories-exist path)
    (uiop:native-namestring path)))

(defvar *latex-previews-tmp-directory*
  (get-cltpt-subdirectory "tmp")
  "path to the directory for intermediate compilation files.")
(defvar *latex-previews-cache-directory*
  (get-cltpt-subdirectory "cache")
  "path to the directory for storing final, cached images.")

(defvar *latex-compiler-command-map*
  '((:latex    . "latex")
    (:pdflatex . "pdflatex")
    (:lualatex . "lualatex -output-format=dvi"))
  "an alist mapping a compiler keyword to the actual command-line program to run.")

(defvar *latex-compiler-key* :latex
  "the keyword identifying the LaTeX compiler to use. see `*latex-compiler-command-map*'.")

;; TODO: dvipng/imagemagick pipelines dont work correctly
(defvar *latex-preview-pipelines*
  `((:dvisvgm
     :description "dvi > svg"
     :image-input-type "dvi"
     :image-output-type "svg"
     :page-number-starts-at 1
     :latex-compiler "%l -interaction=nonstopmode -output-directory %o %f"
     ;; here we use "-%9p.svg" so dvisvgm substitutes a 9-digit, zero-padded page number.
     ;; i personally dont like --bbox=preview because it "hard codes" the width
     :image-converter "dvisvgm --page=1- --bbox=preview --no-fonts --relative --clipjoin --optimize -o %B-%9p.svg %f")
    (:dvipng
     :description "dvi > png"
     :image-input-type "dvi"
     :image-output-type "png"
     :page-number-starts-at 1
     :latex-compiler "%l -interaction=nonstopmode -output-directory %o %f"
     :image-converter "dvipng -T tight -D %D -o %B-%09d.png %f"
     :transparent-image-converter "dvipng -T tight -bg Transparent -D %D -o %B-%9p.png %f")
    (:imagemagick
     :description "pdf > png"
     :image-input-type "pdf"
     :image-output-type "png"
     :page-number-starts-at 0
     :latex-compiler "pdflatex -interaction=nonstopmode -output-directory %o %f"
     :image-converter "convert -density %D -trim -antialias %f -quality 100 %B-%09d.png"))
  "an alist of available LaTeX preview generation pipelines.")

(defvar *latex-preview-pipeline-key*
  :dvisvgm
  "the latex->image pipeline to use from the ones in `*latex-preview-pipelines*'.")
(defvar *preview-filename-prefix*
  "cltpt-snippet-"
  "the prefix to use in temporary files creating for previews.")
(defvar *latex-preview-preamble*
  "\\documentclass[11pt]{article}
\\usepackage{amsmath}
\\usepackage{amssymb}"
  "the preamble used in latex preview compilation.")

(defun get-preamble-source-string ()
  "constructs the full preamble source, including the preview package options."
  (format nil
          "~A~%\\usepackage[active,tightpage]{preview}~%"
          *latex-preview-preamble*))

(defun get-precompiled-preamble-path ()
  "constructs the full path to the precompiled preamble .fmt file.
the filename is based on a hash of the preamble source, ensuring that changes to
the preamble automatically invalidate the old compiled format."
  (let* ((preamble-hash (cltpt/base::md5-str (get-preamble-source-string)))
         (fmt-name (concatenate 'string "preamble-" preamble-hash ".fmt")))
    (merge-pathnames fmt-name *latex-previews-tmp-directory*)))

(defun ensure-cached-format (format-path)
  "ensures the cached preamble format file exists at FORMAT-PATH. if not, generates it."
  (unless (probe-file format-path)
    (let* ((preamble-base-name (pathname-name format-path))
           (preamble-tex-file
             (merge-pathnames
              (concatenate 'string preamble-base-name ".tex")
              *latex-previews-tmp-directory*)))
      (cltpt/file-utils:write-file
       preamble-tex-file
       (format nil "~A\\dump~%" (get-preamble-source-string)))
      ;; TODO: shouldnt we be compiling the preamble in the current dir too?
      ;; but it seems -output-directory isnt supported here?
      (uiop:run-program
       (list "latex"
             "-ini"
             (format nil "-jobname=~A" preamble-base-name)
             "&latex"
             (uiop:native-namestring preamble-tex-file))
       :output t
       :error-output t
       :directory *latex-previews-tmp-directory*)
      (delete-file preamble-tex-file))))

(defun clear-all ()
  "deletes all preview-related files from temporary and cache directories."
  (cltpt/file-utils:delete-files-by-regex
   *latex-previews-tmp-directory*
   "preamble-.*\\.fmt")
  (cltpt/file-utils:delete-files-by-regex
   *latex-previews-tmp-directory*
   *preview-filename-prefix*)
  (cltpt/file-utils:delete-files-by-regex
   *latex-previews-cache-directory*
   *preview-filename-prefix*)
  (format t "~&cleared temporary files, cached previews, and all precompiled preambles.~%"))

(defun format-command (template-string substitutions)
  (let ((result template-string))
    (dolist (sub substitutions result)
      (setf result (cl-ppcre:regex-replace-all (car sub) result (cdr sub))))))

(defun cleanup-temp-files (base-name &optional (intermediate-ext ".dvi"))
  (dolist (ext (list ".tex" ".aux" ".log" ".bcf" ".run.xml" intermediate-ext))
    (let ((path (merge-pathnames
                 (concatenate 'string base-name ext)
                 *latex-previews-tmp-directory*)))
      (when (probe-file path) (delete-file path)))))

;; snippets-count is relevant because imagemagick outputs filepaths that may be
;; postfixed with numbers if its given a document with more than 1 page.
(defun find-generated-file (base-name page-num output-ext tmp-dir pipeline-name snippets-count)
  (cond
    ((and (equal pipeline-name :imagemagick) (= 1 snippets-count))
     (merge-pathnames
      (format nil "~A.~A" base-name output-ext)
      tmp-dir))
    (t
     (merge-pathnames
      (format nil "~A-~9,'0d.~A" base-name page-num output-ext)
      tmp-dir))))

(defun run-compilation-pipeline (snippets-to-compile pipeline-config density transparent)
  "compiles a batch of snippets and renames the output to match their final hashes.
this function now uses a random batch name internally and expects a list of
(hash . snippet-text) cons cells."
  (let* ((use-precomp-p (equal *latex-compiler-key* :latex))
         ;; use a random base name for the temporary batch file to avoid collisions
         (batch-base-name (format nil "batch-~A" (random (expt 2 32))))
         (tmp-dir *latex-previews-tmp-directory*)
         (intermediate-ext (getf pipeline-config :image-input-type))
         (tex-file (merge-pathnames
                    (concatenate 'string batch-base-name ".tex")
                    tmp-dir))
         (intermediate-file
           (merge-pathnames
            (concatenate 'string batch-base-name "." intermediate-ext)
            tmp-dir))
         ;; preamble precompilation is only supported for the :latex compiler.
         (fmt-path (when use-precomp-p (get-precompiled-preamble-path)))
         (compiler-command
          (cdr (assoc *latex-compiler-key*
                      *latex-compiler-command-map*
                      :test 'equal))))
    (unless compiler-command
      (error "unknown compiler key: ~S." *latex-compiler-key*))
    (when use-precomp-p
      (ensure-cached-format fmt-path))
    (cltpt/file-utils:write-file
     tex-file
     (with-output-to-string (out)
       (unless use-precomp-p
         (format out "~A" (get-preamble-source-string)))
       (format out "\\begin{document}~%\\setlength\\abovedisplayskip{0pt}~%")
       (dolist (snippet-cons snippets-to-compile)
         (format out "\\begin{preview}~%~A~%\\end{preview}\\newpage~%"
                 (cdr snippet-cons)))
       (format out "\\end{document}~%")))
    (let* ((final-compiler-command
             (if use-precomp-p
                 (format nil
                         "~A -fmt ~A"
                         compiler-command
                         (uiop:native-namestring fmt-path))
                 compiler-command))
           (latex-template (getf pipeline-config :latex-compiler))
           (substitutions `(("%l" . ,final-compiler-command)
                            ("%o" . ,(uiop:native-namestring tmp-dir))
                            ("%f" . ,(uiop:native-namestring tex-file))))
           (command-str (format-command latex-template substitutions)))
      (uiop:run-program (uiop:split-string command-str :separator " ")
                        ;; :directory *default-pathname-defaults*
                        :output t
                        :error-output t
                        :ignore-error-status t))
    (let* ((converter-template
             (if (and transparent
                      (getf pipeline-config :transparent-image-converter))
                 (getf pipeline-config :transparent-image-converter)
                 (getf pipeline-config :image-converter)))
           (output-basename (merge-pathnames batch-base-name tmp-dir))
           (substitutions `(("%D" . ,(format nil "~A" density))
                            ("%f" . ,(uiop:native-namestring intermediate-file))
                            ("%B" . ,(uiop:native-namestring output-basename))))
           (command-str (format-command converter-template substitutions)))
      (uiop:run-program (uiop:split-string command-str :separator " ")
                        :output t
                        :error-output t
                        :ignore-error-status t))
    ;; rename the numbered output files (...-000..1.svg) to their final hash names in the tmp dir.
    (loop for snippet-cons in snippets-to-compile
          for i from 0
          for page-num = (+ i (getf pipeline-config :page-number-starts-at 0))
          for hash = (car snippet-cons)
          for output-ext = (getf pipeline-config :image-output-type)
          for pipeline-name = (car (rassoc pipeline-config
                                           *latex-preview-pipelines*
                                           :test #'equal))
          for numbered-file = (find-generated-file
                               batch-base-name
                               page-num
                               output-ext
                               tmp-dir
                               pipeline-name
                               (length snippets-to-compile))
          for hashed-file = (merge-pathnames
                             (concatenate 'string
                                          *preview-filename-prefix*
                                          hash
                                          "."
                                          output-ext)
                             tmp-dir)
          do (when (probe-file numbered-file)
               (rename-file numbered-file hashed-file)))
    ;; we dont want to always claean up those temp files, especially not .log files.
    ;; (cleanup-temp-files batch-base-name (concatenate 'string "." intermediate-ext))
    ))

(defun generate-previews-for-latex (snippets
                                    &key
                                      (recompile)
                                      (pipeline *latex-preview-pipeline-key*)
                                      (density 200)
                                      (transparent t))
  "generates image files for a list of LaTeX snippets, compiling only what is needed.
returns an association list of (hash . string-file-path)."
  (unless snippets
    (return-from generate-previews-for-latex nil))
  (cltpt/file-utils:ensure-dir-exists *latex-previews-tmp-directory*)
  (cltpt/file-utils:ensure-dir-exists *latex-previews-cache-directory*)
  (let* ((pipeline-config (cdr (assoc pipeline *latex-preview-pipelines*)))
         (output-ext (getf pipeline-config :image-output-type))
         (cnt 0)
         (use-precomp-p (equal *latex-compiler-key* :latex))
         ;; generate a hash containing all settings so that if any setting changes
         ;; the hash changes and so a recompilation happens.
         (settings-string
           (format nil
                   "compiler-key=~A;precomp=~A;pipeline=~A;density=~A;transparent=~A;preamble-hash=~A"
                   *latex-compiler-key*
                   use-precomp-p
                   pipeline-config
                   density
                   transparent
                   (cltpt/base::md5-str (get-preamble-source-string)))))
    (unless pipeline-config (error "unknown preview pipeline: ~A" pipeline))
    ;; checking the cache
    (let ((missing-snippets)
          (all-snippets-with-hashes))
      (dolist (snippet-text snippets)
        (let* ((hash (cltpt/base::md5-str
                      (concatenate 'string
                                   settings-string
                                   ";snippet="
                                   snippet-text)))
               (file-ext (concatenate 'string "." output-ext))
               (cached-file
                 (merge-pathnames
                  (concatenate 'string *preview-filename-prefix* hash file-ext)
                  *latex-previews-cache-directory*)))
          (push (cons hash snippet-text) all-snippets-with-hashes)
          (unless (and (not recompile) (probe-file cached-file))
            (push (cons hash snippet-text) missing-snippets))))
      (setf all-snippets-with-hashes (nreverse all-snippets-with-hashes))
      (setf missing-snippets (nreverse missing-snippets))
      ;; compilation (only if needed)
      (when missing-snippets
        (run-compilation-pipeline missing-snippets
                                  pipeline-config
                                  density
                                  transparent)
        ;; copy the newly compiled (and now hash-named) files from tmp to cache
        (dolist (snippet-cons missing-snippets)
          (let* ((hash (car snippet-cons))
                 (file-ext (concatenate 'string "." output-ext))
                 (tmp-file
                   (merge-pathnames
                    (concatenate 'string *preview-filename-prefix* hash file-ext)
                    *latex-previews-tmp-directory*))
                 (cached-file
                   (merge-pathnames
                    (concatenate 'string *preview-filename-prefix* hash file-ext)
                    *latex-previews-cache-directory*)))
            (when (probe-file tmp-file)
              (uiop:copy-file tmp-file cached-file)
              ;; (delete-file tmp-file)
              ))))
      ;; return the full list of cache paths
      (mapcar
       (lambda (snippet-cons)
         (let* ((hash (car snippet-cons))
                (file-ext (concatenate 'string "." output-ext)))
           (cons hash
                 (uiop:unix-namestring
                  (merge-pathnames
                   (concatenate 'string
                                *preview-filename-prefix*
                                hash
                                file-ext)
                   *latex-previews-cache-directory*)))))
       all-snippets-with-hashes))))