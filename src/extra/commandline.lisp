(defpackage :cltpt/commandline
  (:use :cl)
  (:export :commandline-main))

(in-package :cltpt/commandline)

(defun top-level-command ()
  (clingon:make-command
   :name "cltpt"
   :description "common lisp text processing tools."
   :version "0.1.0"
   :license "GPL"
   :authors '("Mahmood Sheikh <mahmod.m2015@gmail.com>")
   :handler #'top-level-handler
   :sub-commands (list (convert-command)
                       (roam-command)
                       (agenda-command)
                       (publish-command))
   :options (cli-options)))

(defun cli-options ()
  (list
   (clingon:make-option
    :flag
    :description "help."
    :short-name #\h
    ;; :long-name "help"
    :key :help)))

(defun top-level-handler (cmd)
  (let* (;; (args (clingon:command-arguments cmd))
         ;; (app (clingon:command-parent cmd))
         (to-help (clingon:getopt cmd :help)))
    (cond
      (to-help (clingon:print-usage cmd t))
      )))

;; the current way we do this is problematic because some files might get converted
;; multiple times if they return many nodes (headers etc)
(defun convert-handler (cmd)
  "the handler for the `convert' command"
  (let* ((args (clingon:command-arguments cmd))
         (dest-dir (clingon:getopt cmd :dest-dir))
         (files (clingon:getopt cmd :files))
         (file-rules (mapcar
                      (lambda (r)
                        (read-from-string r))
                      (clingon:getopt cmd :rules)))
         (src-format-name (clingon:getopt cmd :src-format))
         (dest-format-name (clingon:getopt cmd :dest-format))
         (filepath-format (clingon:getopt cmd :filepath-format))
         (static-filepath-format (clingon:getopt cmd :static-filepath-format)))
    (cltpt/utils:convert-all
     :src-format-name src-format-name
     :dest-format-name dest-format-name
     :files files
     :rules file-rules
     :filepath-format filepath-format
     :static-filepath-format static-filepath-format
     :dest-dir dest-dir)))

(defun convert-command ()
  (clingon:make-command
   :name "convert"
   :description "convert files."
   :options (convert-options)
   :handler #'convert-handler))

(defun convert-options ()
  (list
   (clingon:make-option
    :list
    :description "the files to act on."
    :short-name #\f
    :long-name "file"
    :key :files)
   (clingon:make-option
    :list
    :description "the file rules to pass to `cltpt/base:roamer'."
    :short-name #\r
    :long-name "rule"
    :key :rules)
   (clingon:make-option
    :string
    :short-name #\s
    :description "when converting a file, it is possible to provide the intended source format. if unprovided, it will be guessed from the filename extension."
    :long-name "src-format"
    :key :src-format)
   (clingon:make-option
    :string
    :short-name #\d
    :description "the format to convert to."
    :long-name "dest-format"
    :key :dest-format)
   (clingon:make-option
    :string
    :short-name #\o
    :description "the output file name format."
    :long-name "out"
    :key :filepath-format)
   (clingon:make-option
    :string
    :short-name #\c
    :description "the output file name format for \"static\" files."
    :long-name "static-filepath-format"
    :key :static-filepath-format)
   (clingon:make-option
    :string
    :short-name #\g
    :description "the directory to write the converted files to."
    :long-name "dest-dir"
    :key :dest-dir)))

(defun roam-handler (cmd)
  "the handler for the `roam' command"
  (let* ((args (clingon:command-arguments cmd))
         (files (clingon:getopt cmd :files))
         (file-rules (clingon:getopt cmd :rules))
         (output-format (clingon:getopt cmd :output-format))
         (roamer (if file-rules
                     (roamer-from-file-rules file-rules)
                     (when files
                       (cltpt/roam:from-files files)))))
    (when roamer
      (let ((nodes (cltpt/roam:roamer-nodes roamer)))
        (loop for node in nodes
              do (let ((output (cltpt/roam:node-info-format-str node output-format)))
                   (format t "~A~%" output)))))))

(defun roam-command ()
  (clingon:make-command
   :name "roam"
   :description "query information about your files"
   :options (roam-options)
   :handler #'roam-handler))

(defun roam-options ()
  (list
   (clingon:make-option
    :list
    :description "the files to act on."
    :short-name #\i
    :long-name "input"
    :key :files)
   (clingon:make-option
    :list
    :description "the file rules to pass to `cltpt/base:roamer'."
    :short-name #\r
    :long-name "rule"
    :key :rules)
   (clingon:make-option
    :string
    :short-name #\o
    :description "the output format for the info of a roam node."
    :long-name "out"
    :key :output-format)))

(defun agenda-command ()
  (clingon:make-command
   :name "agenda"
   :description "query agenda tree."
   :options (agenda-options)
   :handler #'agenda-handler))

(defun agenda-options ()
  (list
   (clingon:make-option
    :list
    :description "the files to act on."
    :short-name #\i
    :long-name "input"
    :key :files)
   (clingon:make-option
    :list
    :description "the file rules to pass to `cltpt/base:roamer'."
    :short-name #\r
    :long-name "rule"
    :key :rules)
   (clingon:make-option
    :string
    :short-name #\f
    :description "the start time for the agenda."
    :long-name "from"
    :key :from)
   (clingon:make-option
    :string
    :short-name #\t
    :description "the end time for the agenda."
    :long-name "to"
    :key :to)))

(defun date-str-to-ts (date-str)
  ;; wrap the date string in "<>" so it can be parsed as an org timestamp
  (let* ((date-str (format nil "<~A>" date-str))
         (reader (cltpt/combinator:reader-from-input date-str))
         (timestamp-match (cltpt/combinator:apply-rule
                           nil
                           cltpt/org-mode::org-timestamp
                           reader
                           0)))
    (when timestamp-match
      (cltpt/org-mode::org-timestamp-match-to-time date-str timestamp-match))))

(defun agenda-handler (cmd)
  "the handler for the agenda command"
  (let* ((args (clingon:command-arguments cmd))
         (files (clingon:getopt cmd :files))
         (file-rules (clingon:getopt cmd :rules))
         (begin-ts-str (clingon:getopt cmd :from))
         (end-ts-str (clingon:getopt cmd :to))
         (roamer (if file-rules
                     (roamer-from-file-rules file-rules)
                     (when files
                       (cltpt/roam:from-files files)))))
    (when roamer
      (let ((nodes (cltpt/roam:roamer-nodes roamer))
            (agenda (cltpt/agenda:from-roamer roamer))
            (begin-ts (when begin-ts-str
                        (date-str-to-ts begin-ts-str)))
            (end-ts (when end-ts-str
                      (date-str-to-ts end-ts-str))))
        (format t "~A"
                (cltpt/agenda:render-agenda
                 agenda
                 :begin-ts begin-ts
                 :end-ts end-ts))))))

(defun publish-options ()
  (list
   (clingon:make-option
    :list
    :description "org files to publish (repeatable)."
    :short-name #\f
    :long-name "file"
    :key :files)
   (clingon:make-option
    :list
    :description "file rules passed to cltpt/roam:from-files (repeatable)."
    :short-name #\r
    :long-name "rule"
    :key :rules)
   (clingon:make-option
    :string
    :short-name #\g
    :description "the directory to hold the generated site."
    :long-name "dest-dir"
    :required t
    :key :dest-dir)
   (clingon:make-option
    :list
    :description "files to include (repeatable)."
    :short-name #\i
    :long-name "include"
    :key :include-files)
   (clingon:make-option
    :list
    :description "files to exclude from link traversal (repeatable)."
    :short-name #\e
    :long-name "exclude"
    :key :exclude-files)
   (clingon:make-option
    :list
    :description "the template (file) used in converting files to html."
    :short-name #\t
    :long-name "template"
    :key :template)
   (clingon:make-option
    :list
    :description "template files to render into the output dir (repeatable)."
    :short-name #\T
    :long-name "additional-template"
    :key :templates)
   (clingon:make-option
    :string
    :short-name #\o
    :description "the output file name format."
    :long-name "out"
    :key :filepath-format)
   (clingon:make-option
    :string
    :short-name #\c
    :description "the output file name format for \"static\" files."
    :long-name "static-filepath-format"
    :key :static-filepath-format)
   (clingon:make-option
    :string
    :short-name #\m
    :description "theme name to be used."
    :long-name "theme"
    :key :theme)
   (clingon:make-option
    :string
    :short-name #\M
    :description "theme directory to be used."
    :long-name "theme-dir"
    :key :theme-dir)))

(defun publish-handler (cmd)
  (let* ((files         (clingon:getopt cmd :files))
         (file-rules    (mapcar #'read-from-string (clingon:getopt cmd :rules)))
         (dest-dir      (clingon:getopt cmd :dest-dir))
         (include-files (clingon:getopt cmd :include-files))
         (exclude-files (clingon:getopt cmd :exclude-files))
         (templates     (clingon:getopt cmd :templates))
         (template-file (clingon:getopt cmd :template))
         (theme         (clingon:getopt cmd :theme))
         (theme-dir     (clingon:getopt cmd :theme-dir))
         (filepath-format        (clingon:getopt cmd :filepath-format))
         (static-filepath-format (clingon:getopt cmd :static-filepath-format))
         (rmr-files (or file-rules files)))
    ;; resolve theme name to theme-dir if provided
    (when (and theme (not theme-dir))
      (setf theme-dir (cltpt/publish:load-theme-by-name theme)))
    ;; if a theme-dir is set, derive template-file and templates from it
    (when (and theme-dir (not template-file))
      (setf template-file (cltpt/file-utils:join-paths theme-dir "template" "page.html")))
    (when (and theme-dir (not templates))
      (setf templates
            (let ((template-dir (cltpt/file-utils:join-paths theme-dir "template")))
              (loop for path in (uiop:directory-files (cltpt/file-utils:as-dir-path template-dir))
                    when (string= (cltpt/file-utils:file-ext path) "html")
                      collect (uiop:native-namestring path)))))
    ;; copy static assets from theme
    (when theme-dir
      (let ((dest-dir-static (cltpt/file-utils:join-paths dest-dir "static")))
        (cltpt/file-utils:ensure-dir-exists (cltpt/file-utils:as-dir-path dest-dir-static))
        (mapc
         (lambda (item)
           (setf item (uiop:unix-namestring item))
           (uiop:copy-file
            item
            (cltpt/file-utils:join-paths
             dest-dir-static
             (cltpt/file-utils:file-basename item))))
         (uiop:directory-files
          (cltpt/file-utils:as-dir-path (cltpt/file-utils:join-paths theme-dir "static"))))))
    (if rmr-files
        (cltpt/publish:publish
         dest-dir
         rmr-files
         :include-files include-files
         :exclude-files exclude-files
         :templates templates
         :template-file template-file
         :theme-dir theme-dir
         :filepath-format filepath-format
         :static-filepath-format static-filepath-format)
        (progn
          (format *error-output* "error: supply at least one instance of --file or --rule.~%")
          (clingon:print-usage cmd t)))))

(defun publish-command ()
  (clingon:make-command
   :name "publish"
   :description "publish org files as a (static) webapp."
   :options (publish-options)
   :handler #'publish-handler))

(defun commandline-main (argv)
  (let ((app (top-level-command)))
    (clingon:run app argv)))