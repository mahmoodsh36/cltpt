(defpackage :cltpt/tree
  (:use :cl)
  (:export
   :tree-value :tree-children :tree-map :tree-find-all :tree-find
   :is-subtree :tree-root :tree-parent :tree-show :node-depth :has-children))

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

(defgeneric tree-parent (subtree)
  (:documentation "given a (sub)tree (better thought of as a node), return its parnet."))

(defgeneric forest-children (node)
  (:documentation "given a forest (better thought of as a node), return its children (trees)."))

(defgeneric has-children (subtree)
  (:documentation "given a (sub)tree (better thought of as a node), return whether it has children."))

(defmethod tree-value ((subtree cons))
  (car subtree))

(defmethod tree-value ((subtree t))
  (princ-to-string subtree))

(defmethod tree-children ((subtree cons))
  (cdr subtree))

(defmethod forest-children ((forest cons))
  forest)

;; the default 'has-children' should simply return whether 'tree-children' returns non-nil.
(defmethod has-children ((subtree t))
  (tree-children subtree))

(defmethod is-subtree (subtree child)
  "we often only want to iterate through the subtrees if they are of
the same type. e.g. with cons we only want to iterate on a child
if its a cons. this function generalizes this concept. it takes a tree
and another object, returns whether the object should be considered a subtree
to recurse on. the default behavior will be checking whether they are
of the same type, e.g. both are conses."
  (equal (type-of child) (type-of subtree)))

(defun tree-walk (subtree func &key (strategy :dfs) (order :pre-order))
  "walk a tree and apply FUNC to each node.

STRATEGY can be :dfs or :bfs.
ORDER can be :pre-order or :post-order (only applies to :dfs)."
  (case strategy
    (:dfs
     (labels ((walk-dfs (node)
                (when node
                  (when (eq order :pre-order)
                    (funcall func node))
                  (loop for child in (tree-children node)
                        do (when (is-subtree node child)
                             (walk-dfs child)))
                  (when (eq order :post-order)
                    (funcall func node)))))
       (walk-dfs subtree)))
    (:bfs
     (when subtree
       (let ((queue (list subtree)))
         (loop while queue
               do (let ((current-node (pop queue)))
                    (funcall func current-node)
                    (loop for child in (tree-children current-node)
                          do (when (is-subtree current-node child)
                               (setf queue (append queue (list child))))))))))
    (t (error "unknown strategy: ~a. use :dfs or :bfs." strategy))))

(defun tree-map (subtree func &key (order :post-order))
  "maps a tree to a new tree, applying FUNC to each node.

this is a DFS operation.
ORDER can be :pre-order or :post-order.
this function's default behavior is identical to the original."
  (when subtree
    (case order
      (:post-order
       (let ((children-result
               (loop for child in (tree-children subtree)
                     collect (if (is-subtree subtree child)
                                 (tree-map child func :order order)
                                 child))))
         (cons (funcall func subtree) children-result)))
      (:pre-order
       (let ((new-node (funcall func subtree)))
         (cons new-node
               (loop for child in (tree-children subtree)
                     collect (if (is-subtree subtree child)
                                 (tree-map child func :order order)
                                 child)))))
      (t (error "unknown order: ~a. use :pre-order or :post-order." order)))))

(defun tree-find (subtree item
                  &key
                    (test #'equal) (key #'identity)
                    (strategy :dfs) (order :pre-order))
  "find ITEM in SUBTREE.

STRATEGY can be :dfs or :bfs.
ORDER can be :pre-order or :post-order (only applies to :dfs)."
  (tree-walk
   subtree
   (lambda (other-subtree)
     (when (funcall test item (funcall key other-subtree))
       (return-from tree-find other-subtree)))
   :strategy strategy
   :order order)
  nil)

(defun tree-find-all (subtree item
                      &key
                        (test #'equal) (key #'identity)
                        (strategy :dfs) (order :pre-order))
  "find all instances of ITEM in SUBTREE using a configurable traversal.

STRATEGY can be :dfs or :bfs.
ORDER applies to DFS and controls the order of items in the returned list."
  (let ((results '()))
    (tree-walk
     subtree
     (lambda (other-subtree)
       (when (funcall test item (funcall key other-subtree))
         (push other-subtree results)))
     :strategy strategy
     :order order)
    (nreverse results)))

(defun tree-root (subtree)
  "given a tree, return its root. this naturally takes logarithmic time."
  (let ((this-parent (tree-parent subtree)))
    (if this-parent
        (tree-root this-parent)
        subtree)))

(defun tree-show (root-node)
  "displays a tree using abstract accessors and a lsblk-like format."
  (labels ((display-nodes (nodes-list prefix)
             (loop for node in nodes-list
                   ;; check if the current node is the last in the list of siblings
                   for lastp = (null (rest (member node nodes-list)))
                   do (let* ((connector (if lastp "└─" "├─"))
                             (child-prefix (if lastp "  " "│ ")))
                        ;; print the current node's line
                        (format t "~a~a ~a~%" prefix connector (tree-value node))
                        ;; recurse into the children with the new prefix
                        (when (tree-children node)
                          (display-nodes
                           (tree-children node)
                           (concatenate 'string prefix child-prefix)))))))
    (format t "~a~%" (tree-value root-node))
    (display-nodes (tree-children root-node) "")))

(defun node-depth (node)
  "calculate the actual depth of a node based on its position in the tree structure."
  (let ((depth 0)
        (current-node node))
    (loop while (tree-parent current-node)
          do (progn
               (incf depth)
               (setf current-node (tree-parent current-node))))
    depth))

(defun list-to-forest (list)
  "given a list of nodes with their parents and children set, turn the list into a forest."
  (loop for item in list
        if (not (tree-parent item))
          collect item))

(defun trees-map (trees func)
  "iterates through a list of structurally equivalent trees simultaneously
and runs FUNC on the list of corresponding nodes at each step.

children are handled first. the function is always executed on the initial
list of trees. from then on, the function runs on corresponding subtrees as
determined by `is-subtree'."
  (when (some #'identity trees) ;; only proceed if there is at least one non-nil tree
    (let* ((list-of-children (mapcar #'tree-children trees))
           ;; transpose the list of children lists to group corresponding children.
           ;; e.g., ((c1a c1b) (c2a c2b)) -> ((c1a c2a) (c1b c2b))
           (corresponding-children (when list-of-children
                                    (apply #'mapcar #'list list-of-children)))
           (children-result
             (loop for child-group in corresponding-children
                   collect (if (is-subtree (first trees) (first child-group))
                               (trees-map child-group func)
                               child-group))))
      (cons (apply func trees) children-result))))