(defpackage :cltpt/file-utils
  (:use :cl)
  (:export
   :ensure-dir-exists :file-ext
   :file-has-extension-p
   :change-extension :change-dir :path-without-extension
   :file-basename :base-name-no-ext
   :write-file :read-file :join-paths :join-paths-list :walk-dir
   :delete-files-by-glob))

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

(defun write-file (filepath str1)
  (with-open-file (f (uiop:parse-unix-namestring filepath)
                     :direction :output
                     :if-exists :supersede
                     :if-does-not-exist :create)
    (write-sequence str1 f)))

(defun read-file (filepath)
  (uiop:read-file-string filepath))

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

(defun walk-dir (path &key handle-file-fn glob (recurse t))
  "walk a directory and optionally apply HANDLE-FILE-FN to files matching a glob.

args:
  PATH: starting directory (string or pathname).
  HANDLE-FILE-FN: optional function called with each matching file path (string).
  GLOB: glob pattern like \"*.lisp\" or \"**/*.txt\" (defaults to all files).
  RECURSE: when T (default), walks directories recursively. when NIL, only files
           in the top-level directory are considered.

returns a flat list of file paths as strings."
  (let ((path (uiop:ensure-directory-pathname path)))
    (let ((files (uiop:directory-files path))
          (subdirectories (when recurse (uiop:subdirectories path)))
          (matching-files))
      (dolist (file files)
        (when (or (not glob) (pathname-match-p file (merge-pathnames glob path)))
          (let ((file-str (uiop:unix-namestring file)))
            (when handle-file-fn
              (funcall handle-file-fn file-str))
            (push file-str matching-files))))
      ;; recurse into subdirectories (if enabled) and combine results
      (append (nreverse matching-files)
              (loop for dir in subdirectories
                    append (walk-dir dir
                                     :handle-file-fn handle-file-fn
                                     :glob glob
                                     :recurse recurse))))))

(defun delete-files-by-glob (directory-path glob)
  "delete all files in DIRECTORY-PATH whose names match GLOB.
uses UIOP's directory* for glob expansion (supports * and **)."
  (let* ((path (uiop:ensure-directory-pathname directory-path))
         (pattern (merge-pathnames glob path))
         (files (uiop:directory* pattern)))
    (dolist (file files)
      (when (and (probe-file file)
                 (not (uiop:directory-pathname-p file))) ;; skip dirs
        (delete-file file)))))