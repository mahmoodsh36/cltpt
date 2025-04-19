(in-package :cltpt)

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
    :description "directory holding org files."
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
    :string
    :description "export the specified file, the destination format is needed."
    :short-name #\e
    :long-name "export"
    :key :export)
   (clingon:make-option
    :string
    :description "when acting on a file, it is possible to provide the intended source format. if unprovided, it will be guessed from the filename extension."
    :short-name #\f
    :long-name "format"
    :key :format)
   (clingon:make-option
    :flag
    :description "list titles from org files."
    :long-name "list-titles"
    :key :list-titles)))

(defun top-level-handler (cmd)
  (let ((args (clingon:command-arguments cmd))
        (my-dir (clingon:getopt cmd :dir))
        (to-list-titles (clingon:getopt cmd :list-titles))
        (to-help (clingon:getopt cmd :help))
        (src-file (clingon:getopt cmd :file))
        (export-format (clingon:getopt cmd :export))
        (src-format (clingon:getopt cmd :format))
        (app (clingon:command-parent cmd)))
    (if to-help
        (clingon:print-usage cmd t)
        (progn
          (when to-list-titles
            (mapcar 'print (list-org-titles my-dir)))
          (when (and src-file export-format)
            (if src-format
                (progn
                  )
                (progn
                  )))))))

(defun export-file (src dest src-format dest-format)
  )

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