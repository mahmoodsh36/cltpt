(defpackage :cltpt/latex-previews
  (:use :cl)
  (:export
   :*latex-previews-tmp-directory*
   :*latex-previews-cache-directory*
   :preview-directory
   :preview
   :preview-snippet
   :preview-hash
   :preview-path
   :preview-width
   :preview-height
   :preview-depth
   :*latex-compiler-command-map*
   :*latex-compiler-key*
   :*latex-preview-pipelines*
   :*latex-preview-pipeline-key*
   :*preview-filename-prefix*
   :*latex-preview-preamble*
   :get-preamble-source-string
   :get-precompiled-preamble-path
   :ensure-cached-format
   :clear-all
   :format-command
   :cleanup-temp-files
   :find-generated-file
   :run-compilation-pipeline
   :generate-previews-for-latex))

(in-package :cltpt/latex-previews)

(defvar *latex-previews-tmp-directory*
  nil
  "path to the directory for intermediate compilation files. can be absolute or relative.
NIL, the default, means a subdirectory of the temporary directory of the running process, which
is resolved on use rather than here: a directory resolved when this file is loaded is baked into
an image saved by `uiop:dump-image' or save-lisp-and-die, and the temporary directory of whatever
built the image may not exist by the time the image runs.")
(defvar *latex-previews-cache-directory*
  nil
  "path to the directory for storing final, cached images. can be absolute or relative.
NIL, the default, means a subdirectory of the temporary directory of the running process, see
`*latex-previews-tmp-directory*'.")

(defun preview-directory (which)
  "the directory previews use for WHICH, either :tmp or :cache.
the variable set for it, or a subdirectory of the temporary directory of the running process."
  (or (ecase which
        (:tmp *latex-previews-tmp-directory*)
        (:cache *latex-previews-cache-directory*))
      (cltpt/file-utils:as-dir-path
       (cltpt/file-utils:join-paths
        (uiop:native-namestring
         (uiop:merge-pathnames* (make-pathname :directory '(:relative "cltpt-latex-previews"))
                                (uiop:temporary-directory)))
        (string-downcase which)))))

(defvar *latex-compiler-command-map*
  '((:latex    . "latex")
    (:pdflatex . "pdflatex")
    (:lualatex . "lualatex -output-format=dvi"))
  "an alist mapping a compiler keyword to the actual command-line program to run.")

(defvar *latex-compiler-key*
  :latex
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

(defvar *preview-geometry-marker*
  "Preview: Snippet "
  "what a geometry line starts with. the preview package's `lyx' option prints one such line per
snippet, carrying its number, height, depth and width in scaled points. the option names no
dependency, it only makes preview.sty input `prlyx.def', which ships beside preview.sty.")

(defun get-preamble-source-string ()
  "constructs the full preamble source, including the preview package options."
  ;; tightpage crops each page to its snippet and reports the padding it added, lyx prints the
  ;; snippet's dimensions. `parse-preview-geometry' needs both to place an image on the baseline.
  (format nil
          "~A~%\\usepackage[active,tightpage,lyx]{preview}~%"
          *latex-preview-preamble*))

(defun get-precompiled-preamble-path ()
  "constructs the full path to the precompiled preamble .fmt file.
the filename is based on a hash of the preamble source, ensuring that changes to
the preamble automatically invalidate the old compiled format."
  (let* ((preamble-hash (cltpt/str-utils:md5-str (get-preamble-source-string)))
         (fmt-name (concatenate 'string "preamble-" preamble-hash ".fmt")))
    (cltpt/file-utils:join-paths *latex-previews-tmp-directory* fmt-name)))

(defun ensure-cached-format (format-path)
  "ensures the cached preamble format file exists at FORMAT-PATH. if not, generates it."
  (unless (probe-file format-path)
    (let* ((preamble-base-name (pathname-name format-path))
           (preamble-tex-file
             (cltpt/file-utils:join-paths
              *latex-previews-tmp-directory*
              (concatenate 'string preamble-base-name ".tex")))
           ;; NOTE: convert to absolute paths for uiop:run-program.
           ;; the :directory option and file arguments must be absolute paths to work correctly
           ;; when *latex-previews-tmp-directory* is a relative path.
           (abs-tmp-dir (cltpt/file-utils:ensure-absolute *latex-previews-tmp-directory*))
           (abs-tex-file (cltpt/file-utils:ensure-absolute preamble-tex-file)))
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
             (uiop:native-namestring abs-tex-file))
       :output t
       :error-output t
       :directory abs-tmp-dir)
      (delete-file preamble-tex-file))))

(defun clear-all ()
  "deletes all preview-related files from temporary and cache directories."
  (let ((tmp-dir (preview-directory :tmp))
        (cache-dir (preview-directory :cache)))
    ;; delete precompiled preambles
    (cltpt/file-utils:delete-files-by-glob tmp-dir "preamble-*.fmt")
    ;; delete temporary previews
    (cltpt/file-utils:delete-files-by-glob
     tmp-dir
     (format nil "~A*" *preview-filename-prefix*))
    ;; delete cached previews
    (cltpt/file-utils:delete-files-by-glob
     cache-dir
     (format nil "~A*" *preview-filename-prefix*))
    (format t "~&cleared temporary files, cached previews, and all precompiled preambles.~%")))

(defstruct preview
  "one generated preview. PATH is NIL when the snippet did not compile."
  snippet
  hash
  path
  ;; in em of the document the snippet was typeset in, so they say nothing about the size the
  ;; image was rendered at. DEPTH is the part of HEIGHT below the baseline. NIL when the compiler
  ;; reported no geometry.
  width
  height
  depth)

(defun format-command (template-string substitutions)
  (let ((result template-string))
    (dolist (sub substitutions result)
      (setf result (cltpt/str-utils:replace-all result (car sub) (cdr sub))))))

(defun cleanup-temp-files (base-name &optional (intermediate-ext ".dvi"))
  (dolist (ext (list ".tex" ".aux" ".log" ".bcf" ".run.xml" intermediate-ext))
    (let ((path (cltpt/file-utils:join-paths
                 *latex-previews-tmp-directory*
                 (concatenate 'string base-name ext))))
      (when (probe-file path) (delete-file path)))))

;; snippets-count is relevant because imagemagick outputs filepaths that may be
;; postfixed with numbers if its given a document with more than 1 page.
(defun find-generated-file (base-name page-num output-ext tmp-dir pipeline-name snippets-count)
  (cond
    ((and (equal pipeline-name :imagemagick) (= 1 snippets-count))
     (cltpt/file-utils:join-paths
      tmp-dir
      (format nil "~A.~A" base-name output-ext)))
    (t
     (cltpt/file-utils:join-paths
      tmp-dir
      (format nil "~A-~9,'0d.~A" base-name page-num output-ext)))))

(defvar *preview-geometry-ext*
  ".geom"
  "extension of the file holding an image's size and baseline, see `write-preview-geometry'.")

(defvar *preview-tightpage-marker*
  "Preview: Tightpage"
  "what the tightpage option's line of margins starts with.")

(defvar *preview-fontsize-marker*
  "Preview: Fontsize "
  "what the line naming the document's font size starts with. preview.sty prints one per run.")

(defvar *preview-default-fontsize*
  10
  "font size to assume when a run did not print one, latex's own default.")

(defconstant +scaled-points-per-point+
  (* 65536 (/ 72.27d0 72))
  "scaled points to the printer's point: 65536 to the tex point, 72.27 tex points to 72 of these.")

(defun digit-or-sign-p (char)
  (or (digit-char-p char) (char= char #\-)))

(defun parse-integers-in-line (line start count)
  "read COUNT integers out of LINE from START on, skipping whatever separates them.
returns them as a list, or NIL if the line runs out of numbers first."
  (loop :repeat count
        :for pos := start :then next
        :for (value next) := (multiple-value-list
                              (parse-integer
                               line
                               :start (or (position-if #'digit-or-sign-p
                                                       line
                                                       :start pos)
                                          (length line))
                               :junk-allowed t))
        :unless value :do (return nil)
        :collect value))

(defun parse-preview-geometry (log)
  "one (:width W :height H :depth D) plist per snippet in LOG, a compiler run's output, in em and
in the order they appear. three kinds of line are read:

  Preview: Fontsize NNpt                       once per run, the document's font size
  Preview: Tightpage LEFT BOTTOM RIGHT TOP     once per run, from the tightpage option
  Preview: Snippet NUMBER HEIGHT DEPTH WIDTH   once per snippet, from the lyx option

the last two are in scaled points. the snippet line measures the material, the tightpage line the
crop around it, whose LEFT and BOTTOM are negative, the directions it grows in. dividing by the
font size is what makes the result em, and so independent of the size the image is rendered at."
  (let ((geometry)
        (fontsize *preview-default-fontsize*)
        (margins (list 0 0 0 0)))
    (dolist (line (uiop:split-string log :separator '(#\Newline)) (nreverse geometry))
      (let ((fontsize-line (search *preview-fontsize-marker* line))
            (tightpage (search *preview-tightpage-marker* line))
            (snippet (search *preview-geometry-marker* line)))
        (cond
          (fontsize-line
           (let ((size (parse-integers-in-line
                        line
                        (+ fontsize-line (length *preview-fontsize-marker*))
                        1)))
             (when (and size (plusp (first size)))
               (setf fontsize (first size)))))
          (tightpage
           (let ((parsed (parse-integers-in-line
                          line
                          (+ tightpage (length *preview-tightpage-marker*))
                          4)))
             (when parsed
               (setf margins parsed))))
          (snippet
           (let ((numbers (parse-integers-in-line
                           line
                           (+ snippet (length *preview-geometry-marker*))
                           4)))
             (when numbers
               (destructuring-bind (left bottom right top) margins
                 (destructuring-bind (number height depth width) numbers
                   (declare (ignore number))
                   (flet ((em (scaled-points)
                            (/ (max 0 scaled-points)
                               (* +scaled-points-per-point+ fontsize))))
                     (push (list :width (em (- (+ width right) left))
                                 :height (+ (em (+ height top)) (em (- depth bottom)))
                                 :depth (em (- depth bottom)))
                           geometry))))))))))))

(defun preview-geometry-path (directory hash)
  (cltpt/file-utils:join-paths
   directory
   (concatenate 'string *preview-filename-prefix* hash *preview-geometry-ext*)))

(defun write-preview-geometry (directory hash geometry)
  "record GEOMETRY beside the image HASH names, for later runs that find the image cached."
  (cltpt/file-utils:write-file
   (preview-geometry-path directory hash)
   (format nil
           "(:width ~,4F :height ~,4F :depth ~,4F)~%"
           (getf geometry :width)
           (getf geometry :height)
           (getf geometry :depth))))

(defun read-preview-geometry (directory hash)
  "the geometry recorded for HASH, or NIL if there is none.
`ignore-errors' so that a half-written file counts as none rather than signalling."
  (let ((path (probe-file (preview-geometry-path directory hash))))
    (when path
      (ignore-errors
       (read-from-string (cltpt/file-utils:read-file (namestring path)))))))

(defun run-compilation-pipeline (snippets-to-compile pipeline-config density transparent)
  "compiles a batch of snippets and renames the output to match their final hashes.
this function now uses a random batch name internally and expects a list of
(hash . snippet-text) cons cells."
  (let* ((use-precomp-p (equal *latex-compiler-key* :latex))
         ;; a geometry per snippet, filled in from the compiler's output below.
         (geometry)
         ;; use a random base name for the temporary batch file to avoid collisions
         (batch-base-name (format nil "batch-~A" (random (expt 2 32))))
         (tmp-dir *latex-previews-tmp-directory*)
         ;; NOTE: we convert to absolute paths internally for file operations even when the
         ;; user provides relative paths. this is necessary because:
         ;; 1. merge-pathnames with relative paths can cause path doubling issues
         ;; 2. some operations need reliable absolute paths to work correctly
         ;; the relative/absolute nature is preserved in return values from generate-previews-for-latex
         (abs-tmp-dir (cltpt/file-utils:ensure-absolute tmp-dir))
         (intermediate-ext (getf pipeline-config :image-input-type))
         (tex-file (cltpt/file-utils:join-paths
                    abs-tmp-dir
                    (concatenate 'string batch-base-name ".tex")))
         (intermediate-file
           (cltpt/file-utils:join-paths
            abs-tmp-dir
            (concatenate 'string batch-base-name "." intermediate-ext)))
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
                            ("%o" . ,(uiop:native-namestring abs-tmp-dir))
                            ("%f" . ,(uiop:native-namestring tex-file))))
           (command-str (format-command latex-template substitutions))
           (log (uiop:run-program (uiop:split-string command-str :separator " ")
                                  :output '(:string)
                                  :error-output t
                                  :ignore-error-status t)))
      ;; captured rather than let through, so echo it: a failed compilation is diagnosed from it.
      (write-string log *standard-output*)
      (setf geometry (parse-preview-geometry log)))
    (let* ((converter-template
             (if (and transparent
                      (getf pipeline-config :transparent-image-converter))
                 (getf pipeline-config :transparent-image-converter)
                 (getf pipeline-config :image-converter)))
           (output-basename (cltpt/file-utils:join-paths abs-tmp-dir batch-base-name))
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
                               abs-tmp-dir
                               pipeline-name
                               (length snippets-to-compile))
          for hashed-file = (cltpt/file-utils:join-paths
                             abs-tmp-dir
                             (concatenate 'string
                                          *preview-filename-prefix*
                                          hash
                                          "."
                                          output-ext))
          do (when (probe-file numbered-file)
               (rename-file numbered-file hashed-file)
               (let ((snippet-geometry (nth i geometry)))
                 (when snippet-geometry
                   (write-preview-geometry abs-tmp-dir hash snippet-geometry)))))
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
returns a list of `preview' structs, one per snippet in SNIPPETS and in the same order, whose
PATH is NIL for a snippet that produced no image."
  (unless snippets
    (return-from generate-previews-for-latex nil))
  ;; NOTE: we convert to absolute paths internally for all file operations.
  ;; *latex-previews-tmp-directory* is for intermediate compilation files (.tex, .aux, .dvi, .log)
  ;; *latex-previews-cache-directory* is for final hash-named cached images (.svg, .png)
  ;; the workflow is: compile in tmp → convert to images in tmp → copy to cache → return cache paths
  (let* ((*latex-previews-tmp-directory* (preview-directory :tmp))
         (*latex-previews-cache-directory* (preview-directory :cache))
         (abs-tmp-dir (cltpt/file-utils:ensure-absolute *latex-previews-tmp-directory*))
         (abs-cache-dir (cltpt/file-utils:ensure-absolute *latex-previews-cache-directory*))
         (pipeline-config (cdr (assoc pipeline *latex-preview-pipelines*)))
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
                   (cltpt/str-utils:md5-str (get-preamble-source-string)))))
    (unless pipeline-config (error "unknown preview pipeline: ~A" pipeline))
    (cltpt/file-utils:ensure-dir-exists *latex-previews-tmp-directory*)
    (cltpt/file-utils:ensure-dir-exists *latex-previews-cache-directory*)
    ;; checking the cache
    (let ((missing-snippets)
          (all-snippets-with-hashes))
      (dolist (snippet-text snippets)
        (let* ((hash (cltpt/str-utils:md5-str
                      (concatenate 'string
                                   settings-string
                                   ";snippet="
                                   snippet-text)))
               (file-ext (concatenate 'string "." output-ext))
               (cached-file
                 (cltpt/file-utils:join-paths
                  abs-cache-dir
                  (concatenate 'string *preview-filename-prefix* hash file-ext))))
          (push (cons hash snippet-text) all-snippets-with-hashes)
          (unless (and (not recompile)
                       (probe-file cached-file)
                       (read-preview-geometry abs-cache-dir hash))
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
                   (cltpt/file-utils:join-paths
                    abs-tmp-dir
                    (concatenate 'string *preview-filename-prefix* hash file-ext)))
                 (cached-file
                   (cltpt/file-utils:join-paths
                    abs-cache-dir
                    (concatenate 'string *preview-filename-prefix* hash file-ext))))
            (when (probe-file tmp-file)
              (uiop:copy-file tmp-file cached-file)
              ;; the geometry travels with the image, so a run that reuses it still has it.
              (let ((tmp-geometry (probe-file (preview-geometry-path abs-tmp-dir hash))))
                (when tmp-geometry
                  (uiop:copy-file tmp-geometry (preview-geometry-path abs-cache-dir hash))))
              ;; (delete-file tmp-file)
              ))))
      ;; describe what each snippet ended up with.
      (mapcar
       (lambda (snippet-cons)
         (let* ((hash (car snippet-cons))
                (filename (concatenate 'string
                                       *preview-filename-prefix*
                                       hash
                                       "."
                                       output-ext))
                ;; probed for existence only, the path is handed back as built so a relative
                ;; cache directory stays relative.
                (path (cltpt/file-utils:join-paths *latex-previews-cache-directory* filename))
                (compiled (probe-file path)))
           (let ((geometry (when compiled (read-preview-geometry abs-cache-dir hash))))
             (make-preview :snippet (cdr snippet-cons)
                           :hash hash
                           :path (when compiled path)
                           :width (getf geometry :width)
                           :height (getf geometry :height)
                           :depth (getf geometry :depth)))))
       all-snippets-with-hashes))))