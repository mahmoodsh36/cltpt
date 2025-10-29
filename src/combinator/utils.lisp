(defpackage :cltpt/combinator/utils
  (:use :cl)
  (:export
   :find-submatch :find-submatch-all :find-submatch-last
   :match-text :copy-rule :copy-modify-rule))

(in-package :cltpt/combinator/utils)

(defun find-submatch (match submatch-id &optional (test 'string=))
  "from a combinator-returned MATCH, find a sub-match by its SUBMATCH-ID."
  (cltpt/tree:tree-find
   match
   submatch-id
   :key (lambda (node)
          (getf (car node) :id))
   :test test
   :order :post-order))

(defun find-submatch-all (match submatch-id)
  "similar to `find-submatch', but returns all matches."
  (cltpt/tree:tree-find-all
   match
   submatch-id
   :key (lambda (node)
          (getf (car node) :id))
   :order :post-order))

;; TODO: easy to optimize, we dont have to iterate through the whole tree to find the last instance
(defun find-submatch-last (match submatch-id &optional (test 'string=))
  "from a combinator-returned MATCH, find a sub-match by its SUBMATCH-ID. return the last such submatch found."
  (let ((last-found))
    (cltpt/tree:tree-map
     match
     (lambda (submatch)
       (when (funcall test submatch-id (getf (car submatch) :id))
         (setf last-found submatch))))
    last-found))

(defun match-text (match)
  (when match
    (subseq (getf match :str)
            (getf match :begin)
            (getf match :end))))

(defun plistp (list1)
  "check whether LIST1 is a plist."
  (and (consp list1)
       (keywordp (car list1))))

(defun copy-rule (rule id &key type)
  (if (plistp rule)
      (let ((copy (copy-tree rule)))
        (setf (getf copy :id) id)
        (when type
          (setf (getf copy :type) type))
        copy)
      (if type
          (list :pattern (copy-tree rule) :id id :type type)
          (list :pattern (copy-tree rule) :id id))))

(defun copy-modify-rule (rule modifications)
  "copy a RULE, apply MODIFICATIONS to it.

MODIFICATIONS is an alist of the form (id . new-rule) where id is the subrule
to replace and new-rule is the rule to replace it with."
  (let ((new-rule (copy-tree rule)))
    (cltpt/tree:tree-map
     new-rule
     (lambda (subrule)
       (when (plistp subrule)
         (loop for modification in modifications
               for modification-id = (car modification)
               for modification-rule = (cdr modification)
               do (if (equal (getf subrule :id) modification-id)
                      (setf (getf subrule :pattern) modification-rule))))))
    new-rule))