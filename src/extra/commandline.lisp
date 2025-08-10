(defpackage :cltpt/commandline
  (:use :cl :clingon :cltpt/base :cltpt/org-mode)
  (:export :commandline-main))

(in-package :cltpt/commandline)

(defun top-level-command ()
  (clingon:make-command
   :name "cltpt"
   :description "common lisp text processing tools."
   :version "0.1.0"
   :license "MIT"
   :authors '("Mahmood Sheikh <mahmod.m2015@gmail.com>")
   :handler #'top-level-handler
   :sub-commands (list (convert-command) (roam-command))
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

;; (defun convert (node src-format-name dest-format-name)
;;   (labels ((alias-to-name (alias)
;;              (cond
;;                ((string= "org" alias) "org-mode")
;;                ((string= "tex" alias) "latex")
;;                ((string= "md" alias) "markdown")
;;                (otherwise alias)))
;;            (name-to-alias (name)
;;              (cond
;;                ((string= "org-mode" name) "org")
;;                ((string= "latex" name) "tex")
;;                ((string= "markdown" name) "md")
;;                (otherwise name))))
;;     (setf src-format-name (alias-to-name src-format-name))
;;     (setf dest-format-name (alias-to-name dest-format-name))
;;     (let* ((src-file (cltpt/roam:node-file node))
;;            (dest-file (cltpt/base:change-extension
;;                        src-file
;;                        (name-to-alias dest-format-name))))
;;       (format t "converting ~A to ~A~%" src-file dest-file)
;;       (cltpt/base:convert-file
;;        (cltpt/base:text-format-by-name src-format-name)
;;        (cltpt/base:text-format-by-name dest-format-name)
;;        src-file
;;        dest-file))))

(defun roamer-from-file-rules (file-rules)
  (cltpt/roam:from-files
   (mapcar
    (lambda (r)
      (read-from-string r))
    file-rules)))

;; the current way we do this is problematic because some files might get converted
;; multiple times if they return many nodes (headers etc)
(defun convert-handler (cmd)
  "the handler for the `convert' command"
  (let* ((args (clingon:command-arguments cmd))
         (src-format (cltpt/base:text-format-by-name
                      (clingon:getopt cmd :src-format)))
         (dest-format (cltpt/base:text-format-by-name
                      (clingon:getopt cmd :dest-format)))
         (files (clingon:getopt cmd :files))
         (file-rules (clingon:getopt cmd :rules))
         (output-file-format (clingon:getopt cmd :output-file-format))
         (roamer (if file-rules
                     (roamer-from-file-rules file-rules)
                     (cltpt/roam:from-files files))))
    (when (and roamer output-file-format dest-format)
      (cltpt/roam:convert-all roamer dest-format output-file-format))))

(defun convert-command ()
  (clingon:make-command
   :name "convert"
   :description "convert files."
   ;; :usage "[options] [arguments ...]"
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
    :key :output-file-format)))

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
   ;; :usage "[options] [arguments ...]"
   :options (roam-options)
   :handler #'roam-handler))

(defun roam-options ()
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
    :short-name #\o
    :description "the output format for the info of a roam node."
    :long-name "out"
    :key :output-format)))

(defun commandline-main (argv)
  (let ((app (top-level-command)))
    (clingon:run app argv)))