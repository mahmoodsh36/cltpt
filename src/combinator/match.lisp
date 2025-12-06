(defpackage :cltpt/combinator/match
  (:use :cl)
  (:export
   :match-id :match-begin :match-end :match-ctx :match-children
   :make-match :match-clone :match-rule :match-parent
   :match-set-children-parent :match-props :match
   :find-submatch :find-submatch-last :find-submatch-all))

(in-package :cltpt/combinator/match)

(defstruct (match (:print-function
                    (lambda (struct stream depth)
                      (declare (ignore depth))
                      (format stream "#<MATCH [~A:~A]~@[ id:~A~]>"
                              (match-begin struct)
                              (match-end struct)
                              (match-id struct)))))
  children
  parent
  begin
  end
  ctx
  id
  props
  rule)

(defun match-set-children-parent (match)
  "set the parent field of all children to point to MATCH."
  (when (match-children match)
    (dolist (child (match-children match))
      (setf (match-parent child) match)))
  match)

(defun find-submatch (match submatch-id &optional (test 'string=))
  "from a combinator-returned MATCH, find a sub-match by its SUBMATCH-ID."
  (cltpt/tree:tree-find
   match
   submatch-id
   :key (lambda (node)
          (match-id node))
   :test test
   :order :post-order))

(defun find-submatch-all (match submatch-id)
  "similar to `find-submatch', but returns all matches."
  (cltpt/tree:tree-find-all
   match
   submatch-id
   :key (lambda (node)
          (match-id node))
   :order :post-order))

;; TODO: easy to optimize, we dont have to iterate through the whole tree to find the last instance
(defun find-submatch-last (match submatch-id &optional (test 'string=))
  "from a combinator-returned MATCH, find a sub-match by its SUBMATCH-ID. return the last such submatch found."
  (let ((last-found))
    (cltpt/tree:tree-map
     match
     (lambda (submatch)
       (when (funcall test submatch-id (match-id submatch))
         (setf last-found submatch))))
    last-found))

(defun match-clone (match)
  (make-match
   :begin (match-begin match)
   :end (match-end match)
   :ctx (match-ctx match)
   :id (match-id match)
   :parent nil
   :children (mapcar 'match-clone (match-children match))))

(defmethod cltpt/tree:tree-children ((subtree match))
  (match-children subtree))

(defmethod cltpt/tree:tree-parent ((subtree match))
  (match-parent subtree))

(defmethod cltpt/tree:tree-value ((subtree match))
  subtree)