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
                       (agenda-command))
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

(defun commandline-main (argv)
  (let ((app (top-level-command)))
    (clingon:run app argv)))