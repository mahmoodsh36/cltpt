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
   :options (cli-options)))

(defun cli-options ()
  (list
   (clingon:make-option
    :flag
    :description "help."
    :short-name #\h
    ;; :long-name "help"
    :key :help)
   (clingon:make-option
    :list
    :description "the files to act on."
    :short-name #\f
    :long-name "file"
    :key :files)
   (clingon:make-option
    :string
    :description "when acting on a file, it is possible to provide the intended source format. if unprovided, it will be guessed from the filename extension."
    :long-name "src-format"
    :key :src-format)
   (clingon:make-option
    :string
    :description "action to run on specified file."
    :long-name "action"
    :key :action)
   (clingon:make-option
    :list
    :description "arguments to pass to function running the specified action."
    :long-name "action-arg"
    :short-name #\a
    :key :action-args)))

(defun top-level-handler (cmd)
  (let* (;; (args (clingon:command-arguments cmd))
         ;; (app (clingon:command-parent cmd))
         (action-args (when (clingon:getopt cmd :action-args)
                        (clingon:getopt cmd :action-args)))
         (to-help (clingon:getopt cmd :help))
         (action (clingon:getopt cmd :action))
         (files (clingon:getopt cmd :files)))
    (cond
      (to-help (clingon:print-usage cmd t))
      (action
       (let* ((rmr (cltpt/roam:from-files
                    `((:path ,files
                       :regex ".*\\.org"
                       :format "org-mode"))))
              (nodes (cltpt/roam:roamer-nodes rmr)))
         (loop for node in nodes
               do (apply (intern (string-upcase action) :cltpt/commandline)
                         (cons node
                               action-args))))))))

(defun show-info (node format-str)
  (cltpt/base:bind-and-eval
   `((title ,(cltpt/roam:node-title node))
     (file ,(cltpt/roam:node-file node))
     (id ,(cltpt/roam:node-id node)))
   (lambda ()
     ;; need to use in-package to access the variables bound above
     (in-package :cltpt/commandline)
     (let ((result
             (cltpt/base:convert-tree
              (cltpt/base:parse
               format-str
               (list 'cltpt/base:text-macro 'cltpt/base:post-lexer-text-macro))
              (cltpt/base:text-format-by-name "latex") ;; just use latex for now
              (list 'cltpt/base:text-macro 'cltpt/base:post-lexer-text-macro))))
       (format t "~A~%" result)))))

;; this is problematic because it may convert the same file multiple times
;; if the file contains multiple roam nodes (e.g. headers etc)
(defun convert (node src-format-name dest-format-name)
  (labels ((alias-to-name (alias)
             (cond
               ((string= "org" alias) "org-mode")
               ((string= "tex" alias) "latex")
               ((string= "md" alias) "markdown")
               (otherwise alias)))
           (name-to-alias (name)
             (cond
               ((string= "org-mode" name) "org")
               ((string= "latex" name) "tex")
               ((string= "markdown" name) "md")
               (otherwise name))))
    (setf src-format-name (alias-to-name src-format-name))
    (setf dest-format-name (alias-to-name dest-format-name))
    (let* ((src-file (cltpt/roam:node-file node))
           (dest-file (cltpt/base:change-extension
                       src-file
                       (name-to-alias dest-format-name))))
      (format t "converting ~A to ~A~%" src-file dest-file)
      (cltpt/base:convert-file
       (cltpt/base:text-format-by-name src-format-name)
       (cltpt/base:text-format-by-name dest-format-name)
       src-file
       dest-file))))

(defun commandline-main (argv)
  (let ((app (top-level-command)))
    (clingon:run app argv)))