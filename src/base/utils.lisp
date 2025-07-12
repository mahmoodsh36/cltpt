(in-package :cltpt/base)

(defun last-atom (seq)
  (car (last seq)))

(defun ensure-directory (dir)
  (unless (probe-file dir)
    (uiop:ensure-directory-pathname dir)))

(defun md5-str (s)
  (ironclad:byte-array-to-hex-string
   (ironclad:digest-sequence
    :md5
    (ironclad:ascii-string-to-byte-array s))))

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

(defun tree-mapcar (node func &optional cond)
  "we iterate through the tree one NODE at a time and run FUNC on each, COND
decides whether to recurse on a specific node. a tree is returned with nodes
replaced by the results of calling FUNC on them. children are handled first."
  (if cond
      (if (and (consp node) (funcall cond node))
          (cons (funcall func (car node))
                (loop for child in (cdr node)
                      collect (tree-mapcar child func cond)))
          (cons (funcall func (car node))
                (cdr node)))
      (if (consp node)
          (cons (funcall func (car node))
                (loop for child in (cdr node)
                      collect (tree-mapcar child func cond)))
          node)))

(defun tree-find (node item &key (test #'equal) (key #'identity))
  "find `ITEM' from the `car' of some node in the tree NODE.
TEST checks for equality between ITEM and `(key (car node))'."
  (tree-mapcar
   node
   (lambda (other-node)
     (when (funcall test item (funcall key other-node))
       (return-from tree-find other-node))))
  nil)

(defun tree-find-all (node item &key (test #'equal) (key #'identity))
  "similar to `tree-find' but returns all instances matched from the tree."
  (let ((result))
    (tree-mapcar
     node
     (lambda (other-node)
       (when (funcall test item (funcall key other-node))
         (push other-node result))))
    result))

(defun find-submatch (match submatch-id)
  "from a combinator-returned MATCH, find a sub-match by its SUBMATCH-ID."
  (tree-find
   match
   submatch-id
   :key (lambda (node)
          (getf node :id))))

(defun find-submatch-all (match submatch-id)
  "similar to `find-submatch', but returns all matches."
  (tree-find-all
   match
   submatch-id
   :key (lambda (node)
          (getf node :id))))

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