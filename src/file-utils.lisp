(defpackage :cltpt/file-utils
  (:use :cl)
  (:export
   :ensure-dir-exists :file-ext
   :file-has-extension-p
   :change-extension :change-dir :path-without-extension
   :file-basename :base-name-no-ext
   :write-file :read-file :join-paths :join-paths-list :walk-dir :as-dir-path
   :delete-files-by-glob :ensure-filepath-pathname :ensure-filepath-string :ensure-absolute
   :temp-file))

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
  "return PATH but with its directory replaced by NEW-DIR."
  (let* ((p (uiop:ensure-pathname path :want-existing nil))
         (dir (uiop:ensure-directory-pathname new-dir))
         ;; extract just the file part of PATH
         (file-part (make-pathname
                     :name (pathname-name p)
                     :type (pathname-type p)
                     :version (pathname-version p))))
    (uiop:native-namestring
     (uiop:merge-pathnames* file-part dir))))

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

(defun ensure-filepath-string (fp)
  (if (typep fp 'pathname)
      (uiop:unix-namestring fp)
      fp))

(defun ensure-filepath-pathname (fp)
  (if (typep fp 'string)
      fp
      (pathname fp)))

(defun as-dir-path (path)
  "ensure PATH is a directory path ending with the appropriate directory separator."
  (let* ((path-str (ensure-filepath-string path))
         (sep (uiop:directory-separator-for-host)))
    (if (and (> (length path-str) 0)
             (char= (char path-str (1- (length path-str))) sep))
        path-str
        (concatenate 'string path-str (string sep)))))

(defun ensure-absolute (path)
  "ensures PATH is absolute."
  (ensure-filepath-string
   (uiop:ensure-absolute-pathname
    path
    (uiop:ensure-directory-pathname *default-pathname-defaults*))))

(defun random-string (&optional (length 12))
  (let ((chars "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"))
    (coerce
     (loop repeat length
           collect (elt chars (random (length chars))))
     'string)))

(defun temp-file (&optional (prefix "cltpt") postfix)
  (let* ((dir (uiop:temporary-directory))
         (rand (random-string 12))
         (name (if postfix
                   (format nil "~A-~A.~A" prefix rand postfix)
                   (format nil "~A-~A" prefix rand))))
    (cltpt/file-utils:join-paths
     (cltpt/file-utils:ensure-filepath-string dir)
     name)))