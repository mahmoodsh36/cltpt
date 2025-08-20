(defpackage :cltpt/combinator/utils
  (:use :cl)
  (:export :find-submatch :find-submatch-all :find-submatch-last))

(in-package :cltpt/combinator/utils)

(defun find-submatch (match submatch-id)
  "from a combinator-returned MATCH, find a sub-match by its SUBMATCH-ID."
  (cltpt/tree:tree-find
   match
   submatch-id
   :key (lambda (node)
          (getf (car node) :id))))

;; TODO: easy to optimize, we dont have to iterate through the whole tree to find the last instance
(defun find-submatch-last (match submatch-id)
  "from a combinator-returned MATCH, find a sub-match by its SUBMATCH-ID. return the last such submatch found."
  (let ((last-found))
    (cltpt/tree:tree-map
     match
     (lambda (submatch)
       (when (equal submatch-id (getf (car submatch) :id))
         (setf last-found submatch))))
    last-found))

(defun find-submatch-all (match submatch-id)
  "similar to `find-submatch', but returns all matches."
  (cltpt/tree:tree-find-all
   match
   submatch-id
   :key (lambda (node)
          (getf (car node) :id))))