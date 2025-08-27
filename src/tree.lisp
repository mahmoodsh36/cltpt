(defpackage :cltpt/tree
  (:use :cl)
  (:export :tree-value :tree-children :tree-map :tree-find-all :tree-find
           :is-subtree))

(in-package :cltpt/tree)

;; a module that abstracts functionality of different kinds of trees

(defgeneric tree-value (subtree)
  (:documentation "given a (sub)tree, return the value associated with its root.

for a tree represented by a cons 'x', the 'value' might be '(car x)', while
the children are represented by '(cdr x)'. for other structures, this may
differ. we differentiate between the subtree (also called node) itself
and its value using the generic 'tree-value' function, which takes a subtree
and returns the value associated with the node at its root."))

(defgeneric tree-children (subtree)
  (:documentation "given a (sub)tree (better thought of as a node), return its children. this shouldnt be recursive."))

(defmethod tree-value ((subtree cons))
  (car subtree))

(defmethod tree-children ((subtree cons))
  (cdr subtree))

(defmethod is-subtree (subtree child)
  "we often only want to iterate through the subtrees if they are of
the same type. e.g. with cons we only want to iterate on a child
if its a cons. this function generalizes this concept. it takes a tree
and another object, returns whether the object should be considered a subtree
to recurse on. the default behavior will be checking whether they are
of the same type, e.g. both are conses."
  (equal (type-of child) (type-of subtree)))

(defun tree-map (subtree func)
  "we iterate through the tree one subtree at a time and run FUNC on each.

children are handled first. the function will always be executed on the initial
subtree even if its not an actual tree (unless its `nil'). from then on the
function will run only on subtrees as determined by `is-subtree'."
  (when subtree
    (let ((children-result
            (loop for child in (tree-children subtree)
                  collect (if (is-subtree subtree child)
                              (tree-map child func)
                              child))))
      ;; (when (typep subtree 'cltpt/base:text-object)
      ;;   (format t "here66 ~A~%" subtree))
      (cons (funcall func subtree)
            children-result))))

(defun tree-find (subtree item
                  &key (test #'equal) (key #'identity))
  "find `ITEM' from in `SUBTREE'.

TEST checks for equality between ITEM and `(key SUBTREE)'."
  (tree-map
   subtree
   (lambda (other-subtree)
     (when (funcall test
                    item
                    (funcall key other-subtree))
       (return-from tree-find other-subtree))))
  nil)

(defun tree-find-all (subtree item
                      &key (test #'equal) (key #'identity))
  "similar to `tree-find' but returns all instances matched from the tree."
  (let ((result))
    (tree-map
     subtree
     (lambda (other-subtree)
       (when (funcall test item (funcall key other-subtree))
         (push other-subtree result))))
    result))