(defpackage :cltpt/file-utils
  (:use :cl)
  (:export
   :ensure-dir-exists :file-ext
   :file-has-extension-p
   :change-extension :change-dir :path-without-extension
   :file-basename :base-name-no-ext :delete-files-by-regex
   :write-file :join-paths :join-paths-list :walk-dir))

(in-package :cltpt/file-utils)

(defun ensure-dir-exists (dir)
  "ensure directory DIR exists."
  (unless (probe-file dir)
    (ensure-directories-exist dir)))

(defun file-ext (filepath)
  "return the file extension of FILEPATH."
  (string-downcase (pathname-type filepath)))

(defun file-has-extension-p (path exts)
  "check if PATH ends with one of the extensions in EXTS."
  (let ((type (string-downcase (pathname-type path))))
    (some (lambda (ext)
            (string-equal type (string-downcase ext)))
          exts)))

(defun change-extension (path new-ext)
  "change the extension of PATH to NEW-EXT."
  (let* ((pathname (pathname path))
         (name (file-namestring (make-pathname :name (pathname-name pathname)
                                               :type new-ext))))
    (namestring (merge-pathnames name (pathname path)))))

(defun change-dir (path new-dir)
  "change the directory of PATH to NEW-DIR."
  (let ((file-part (make-pathname :name (pathname-name path)
                                  :type (pathname-type path)
                                  :version (pathname-version path)))
        (dir-part (make-pathname :name nil :type nil :version nil
                                 :defaults new-dir)))
    (uiop:unix-namestring (uiop:merge-pathnames* file-part dir-part))))

(defun path-without-extension (path)
  (let* ((pathname (pathname path))
         (dir (pathname-directory pathname))
         (name (pathname-name pathname)))
    (namestring (make-pathname :directory dir :name name :type nil))))

(defun file-basename (path)
  (namestring (make-pathname :name (pathname-name path) :type (pathname-type path))))

(defun base-name-no-ext (path)
  (namestring (make-pathname :name (pathname-name path))))

(defun delete-files-by-regex (directory-path regex)
  "deletes files in a directory whose names matches REGEX."
  (let ((all-files (directory (merge-pathnames "*.*" directory-path))))
    (loop for file in all-files
          do (when (cl-ppcre:scan regex (file-namestring file))
               (delete-file file)))))

(defun write-file (filepath str1)
  (with-open-file (f (uiop:parse-unix-namestring filepath)
                     :direction :output
                     :if-exists :supersede
                     :if-does-not-exist :create)
    (write-sequence str1 f)))

(defun join-paths (&rest components)
  "merge given args as filepaths, see `join-paths-list'."
  (join-paths-list components))

(defun join-paths-list (components &key (separator "/"))
  "merge given filepaths in the COMPONENTS list from left to right.

all paths are considered to be directory names except the last one
which is the name of the file. the function returns a string.
the function trims excessive use of the separator (usually forward slash)."
  (let ((clean-parts (loop for part in components
                           for trimmed = (string-trim separator part)
                           unless (string= trimmed "")
                           collect trimmed)))
    (when clean-parts
      (with-output-to-string (s)
        ;; handle absolute paths: if the first original component started
        ;; with a separator, the new path should too.
        (when (and (first components)
                   (> (length (first components)) 0)
                   (char= (char (first components) 0) (char separator 0)))
          (write-string separator s))
        ;; write the first part, then loop through the rest,
        ;; adding a separator each time.
        (write-string (first clean-parts) s)
        (dolist (part (rest clean-parts))
          (write-string separator s)
          (write-string part s))))))

(defun walk-dir (path handle-file-fn regex &optional (recursive t))
  "walks a directory and applies a function to files matching a regex.

args:
  path: the starting directory (a pathname or a string).
  handle-file-fn: a function to call with the pathname of each matching file.
  regex: a CL-PPCRE regular expression string to match against file names.
  recursive: when T (the default), walks the directory tree recursively.
             when NIL, only processes files directly within PATH."
  (labels ((process-files-in-directory (dir)
             (dolist (file (uiop:directory-files dir))
               ;; check if the file's name matches the regex.
               (when (cl-ppcre:scan regex (namestring file))
                 (funcall handle-file-fn file)))))
    (if recursive
        (labels ((walk (current-dir)
                   ;; process files in the current directory first.
                   (process-files-in-directory current-dir)
                   ;; get all subdirectories and recurse into each.
                   (dolist (subdir (uiop:subdirectories current-dir))
                     (walk subdir))))
          (walk path))
        (process-files-in-directory path))))