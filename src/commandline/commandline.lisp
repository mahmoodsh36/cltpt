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
    :string
    :description "directory holding files."
    :short-name #\d
    :long-name "dir"
    :initial-value (uiop:native-namestring "~/notes/")
    :key :dir)
   (clingon:make-option
    :string
    :description "the file to act on."
    :short-name #\f
    :long-name "file"
    :key :file)
   (clingon:make-option
    :flag
    :description "choose to convert the file specified with `-f`."
    :short-name #\c
    :long-name "convert"
    :key :convert)
   (clingon:make-option
    :string
    :description "when acting on a file, it is possible to provide the intended source format. if unprovided, it will be guessed from the filename extension."
    :long-name "src-format"
    :key :src-format)
   (clingon:make-option
    :string
    :description "destination format when converting."
    :long-name "dest-format"
    :key :dest-format)
   (clingon:make-option
    :string
    :description "destination filepath when converting."
    :long-name "dest-file"
    :short-name #\t
    :key :dest-file)
   (clingon:make-option
    :flag
    :description "action to run on specified file."
    :long-name "action"
    :key :action)))

(defun infer-format-name-from-filepath (fp)
  (let ((ext (pathname-type (pathname fp))))
    (cond
      ((equal ext "org")
       cltpt/org-mode:org-mode)
      ((equal ext "tex")
       cltpt/latex:latex))))

(defun top-level-handler (cmd)
  (let* ((args (clingon:command-arguments cmd))
         (my-dir (clingon:getopt cmd :dir))
         (to-list-titles (clingon:getopt cmd :list-titles))
         (to-help (clingon:getopt cmd :help))
         (src-file (clingon:getopt cmd :file))
         (to-convert (clingon:getopt cmd :convert))
         (dest-file (clingon:getopt cmd :dest-file))
         (dest-format (or (text-format-by-name (clingon:getopt cmd :dest-format))
                          (and dest-file (infer-format-name-from-filepath dest-file))))
         (src-format (or (text-format-by-name (clingon:getopt cmd :src-format))
                         (and src-file (infer-format-name-from-filepath src-file))))
         (app (clingon:command-parent cmd)))
    (if to-help
        (clingon:print-usage cmd t)
        (progn
          (when to-list-titles
            (mapcar 'print (list-org-titles my-dir)))
          (when to-convert
            (convert-file src-format dest-format src-file dest-file))))))

(defun commandline-main (argv)
  (let ((app (top-level-command)))
    (clingon:run app argv)))

(defun list-org-files (dir)
  (directory (format nil "~A*.org" dir)))

(defun list-org-titles (dir)
  (time
   (let ((object-types (list (find-class 'org-keyword))))
     ;; needed for MOP to work
     (mapcar 'sb-mop:finalize-inheritance object-types)
     (remove-if-not
      'identity
      (loop for org-file in (list-org-files dir)
            collect (let* ((result (parse (uiop:read-file-string org-file)
                                          object-types
                                          :as-doc nil))
                           (title))
                      (mapc
                       (lambda (entry)
                         (when (string= (text-object-property entry :keyword) "title")
                           (setf title (text-object-property entry :value))))
                       result)
                      title))))))