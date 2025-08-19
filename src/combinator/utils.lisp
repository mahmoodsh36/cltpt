(defpackage :cltpt/combinator/utils
  (:use :cl)
  (:export :find-submatch :find-submatch-all :find-submatch-last))

(in-package :cltpt/combinator/utils)

(defun tree-mapcons (node func)
  "we iterate through the tree one subtree at a time and run FUNC on each.
children are handled first."
  (when (consp node)
    (progn
      (loop for child in (cdr node)
            do (tree-mapcons child func))
      (funcall func node))))

(defun tree-find (node item &key (test #'equal) (key #'identity))
  "find `ITEM' from the `car' of some node in the tree NODE.
TEST checks for equality between ITEM and `(key (car node))'."
  (tree-mapcons
   node
   (lambda (other-node)
     (when (funcall test item (funcall key other-node))
       (return-from tree-find other-node))))
  nil)

(defun tree-find-all (node item &key (test #'equal) (key #'identity))
  "similar to `tree-find' but returns all instances matched from the tree."
  (let ((result))
    (tree-mapcons
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
          (getf (car node) :id))))

;; TODO: easy to optimize, we dont have to iterate through the whole tree to find the last instance
(defun find-submatch-last (match submatch-id)
  "from a combinator-returned MATCH, find a sub-match by its SUBMATCH-ID. return the last such submatch found."
  (let ((last-found))
    (tree-mapcons
     match
     (lambda (submatch)
       (when (equal submatch-id (getf (car submatch) :id))
         (setf last-found submatch))))
    last-found))

(defun find-submatch-all (match submatch-id)
  "similar to `find-submatch', but returns all matches."
  (tree-find-all
   match
   submatch-id
   :key (lambda (node)
          (getf (car node) :id))))