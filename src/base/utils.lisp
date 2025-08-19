(in-package :cltpt/base)

(defun last-atom (seq)
  (car (last seq)))

(defun ensure-directory (dir)
  (unless (probe-file dir)
    (ensure-directories-exist dir)))

(defun md5-str (s)
  (ironclad:byte-array-to-hex-string
   (ironclad:digest-sequence
    :md5
    (sb-ext:string-to-octets s :external-format :utf-8))))

;; this is a temporary workaround because `find-class' is really slow..
(defparameter *class-map* (make-hash-table))
(defun find-class-faster (class-sym)
  (let ((result (gethash class-sym *class-map*)))
    (unless result
      (setf result (find-class class-sym))
      (setf (gethash class-sym *class-map*) result))
    result))

(defun bind-and-eval (bindings func)
  "dynamically binds symbols from BINDINGS (a list of symbol,value pairs) and executes FUNC."
  (let ((keys (mapcar #'car bindings))
        (values (mapcar #'eval (mapcar #'cadr bindings))))
    (progv keys values
      (funcall func))))
(defun bind-and-eval* (bindings func)
  (if (null bindings)
      (funcall func)
    (destructuring-bind (sym val-expr) (car bindings)
      (progv (list sym) (list (eval val-expr))
        (bind-and-eval* (cdr bindings) func)))))
;; example
;; (bind-and-eval '((x 1) (y (+ 1 2))) (lambda () (+ x y)))
;; (bind-and-eval* '((x 1) (y (+ x 2))) (lambda () (+ x y)))

(defmacro pcase (keyform &body clauses)
  "a case matcher that can take variables, like elisp's `pcase'.

example usage: `(let ((myvar 'latex)) (pcase 'latex ('html 1) (myvar 2)))'"
  (let ((keyval-gensym (gensym "pcase")))
    `(let ((,keyval-gensym ,keyform))
       (cond
         ,@(loop for clause in clauses
                 if (member (car clause) '(t otherwise) :test #'eq)
                   collect `(t ,@(cdr clause))
                 else
                   collect (let ((test-form (car clause))
                                 (body (cdr clause)))
                             `((eql ,keyval-gensym ,test-form)
                               ,@body)))))))

(defun plistp (list1)
  "check whether LIST1 is a plist."
  (and (consp list1)
       (keywordp (car list1))))

(defun flatten (l)
  "flatten a tree: turn it into a list."
  (cond ((null l) nil)
        ((atom l) (list l))
        (t (loop for a in l appending (flatten a)))))

;; ended up not needing this more complex one
;; (defun flatten (tree &optional (max-depth 1))
;;   "flatten TREE so that no node in the result exceeds MAX-DEPTH nesting.
;; example: (flatten '(1 (2 (3 4)) (((5 10)))) 2) => (1 2 (3 4) (5 10))"
;;   (labels
;;       ((node-depth (node)
;;          (if (atom node)
;;              0
;;              (1+ (reduce 'max (mapcar #'node-depth node)))))
;;        (flatten-if-too-deep (node)
;;          (if (or (atom node)
;;                  (<= (node-depth node) max-depth))
;;              node
;;              (mapcan (lambda (x)
;;                        (let ((res (flatten-if-too-deep x)))
;;                          (if (listp res) res (list res))))
;;                      node))))
;;     (flatten-if-too-deep tree)))

(defun file-ext (filepath)
  (string-downcase (pathname-type filepath)))

(defun file-has-extension-p (path exts)
  "check if PATH ends with one of the extensions in EXTS."
  (let ((type (string-downcase (pathname-type path))))
    (some (lambda (ext)
            (string-equal type (string-downcase ext)))
          exts)))

(defun directory-files-matching (dir regex)
  "list files in a directory DIR that match a specific REGEX."
  (let ((files (uiop:directory-files dir)))
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

(defun compress-consec (s char-to-compress)
  "compresses runs of a specific character into a single instance. aaab -> ab."
  (with-output-to-string (out)
    (loop for current-char across s
          with last-char = nil
          do
             (unless (and (char= current-char char-to-compress)
                          (and last-char (char= last-char char-to-compress)))
               (write-char current-char out))
             (setf last-char current-char))))