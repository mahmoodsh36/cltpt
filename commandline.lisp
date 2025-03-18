(in-package :cltpt)

(defun top-level-command ()
  (clingon:make-command
   :name "cltpt"
   :description "common lisp text processing tools."
   :version "0.1.0"
   :license "GPL-3.0-or-later"
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
    :string
    :description "directory holding org files."
    :short-name #\d
    :long-name "dir"
    :initial-value (uiop:native-namestring "~/notes/")
    :key :dir)
   (clingon:make-option
    :flag
    :description "list titles from org files."
    :long-name "list-titles"
    :key :dir)))

(defun top-level-handler (cmd)
  (let ((args (clingon:command-arguments cmd))
        (my-dir (clingon:getopt cmd :dir))
        (to-list-titles (clingon:getopt cmd :dir))
        (to-help (clingon:getopt cmd :help))
        (app (clingon:command-parent cmd)))
    (if to-help
        (clingon:print-usage cmd t)
        (when to-list-titles
          (mapcar 'print (grab-titles))))))

(defun commandline-main (argv)
  (let ((app (top-level-command)))
    (clingon:run app argv)))