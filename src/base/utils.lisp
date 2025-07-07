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
  (tree-mapcar
   node
   (lambda (other-node)
     (when (funcall test item (funcall key other-node))
       (return-from tree-find other-node))))
  nil)

(defun find-submatch (match submatch-id)
  (cltpt/base::tree-find
   match
   submatch-id
   :key (lambda (node)
          (getf node :id))))

(defun flatten (l)
  (cond ((null l) nil)
        ((atom l) (list l))
        (t (loop for a in l appending (flatten a)))))