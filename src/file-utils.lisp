(defpackage :cltpt/file-utils
  (:use :cl)
  (:export
   :ensure-directory :file-ext
   :file-has-extension-p :directory-files-matching
   :change-extension :change-dir :path-without-extension
   :file-basename :base-name-no-ext :delete-files-by-regex
   :write-file :join-paths :join-paths-list))

(in-package :cltpt/file-utils)

(defun ensure-directory (dir)
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

(defun directory-files-matching (dir regex)
  "list files in a directory DIR that match a specific REGEX."
  (let ((files (mapcar 'uiop:unix-namestring (uiop:directory-files dir))))
    (if regex
        (remove-if-not
         (lambda (path)
           (cl-ppcre:scan regex (namestring path)))
         files)
        files)))

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
    (merge-pathnames file-part dir-part)))

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