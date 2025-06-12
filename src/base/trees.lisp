(in-package :cltpt/base)

(defun make-node (interval)
  (cons interval nil))

(defun intervals-conflict-p (c-start c-end p-start p-end)
  "checks if two intervals (c-start, c-end) and (p-start, p-end)
overlap AND neither contains the other."
  (let ((overlap (< (max c-start p-start) (min c-end p-end))))
    (when overlap
      (let ((c-contains-p (and (<= c-start p-start) (>= c-end p-end)))
            (p-contains-c (and (<= p-start c-start) (>= p-end c-end))))
        (not (or c-contains-p p-contains-c))))))

(defun build-forest (intervals)
  "build a nested tree from a list of intervals, discarding entries
that overlap with a sibling or a root without being contained by it (or containing it).
each interval is a list of the form (start end id)."
  (let ((forest)
        (stack))
    (setf intervals (sort (copy-list intervals) #'< :key #'first))
    (dolist (current-interval-data intervals)
      (let ((node-to-add (make-node current-interval-data))
            (c-start (first current-interval-data))
            (c-end (second current-interval-data))
            (discard-current))
        (loop while (and stack
                         (let* ((parent-node-on-stack (first stack))
                                (p-interval-on-stack (car parent-node-on-stack))
                                (p-start (first p-interval-on-stack))
                                (p-end (second p-interval-on-stack)))
                           (not (and (<= p-start c-start) (>= p-end c-end)))))
              do (pop stack))
        (let ((potential-parent-node (first stack)))
          (if potential-parent-node
              (dolist (sibling-node (cdr potential-parent-node))
                (when (not discard-current)
                  (let* ((s-interval-data (car sibling-node))
                         (s-start (first s-interval-data))
                         (s-end (second s-interval-data)))
                    (when (intervals-conflict-p c-start c-end s-start s-end)
                      (setf discard-current t)))))
              (dolist (existing-root-node forest)
                (when (not discard-current)
                  (let* ((r-interval-data (car existing-root-node))
                         (r-start (first r-interval-data))
                         (r-end (second r-interval-data)))
                    (when (intervals-conflict-p c-start c-end r-start r-end)
                      (setf discard-current t)))))))
        (unless discard-current
          (if (first stack)
              (progn
                (push node-to-add (cdr (first stack)))
                (push node-to-add stack))
              (progn
                (push node-to-add forest)
                (push node-to-add stack))))))
    (nreverse forest)))

(defun print-node (node &optional (indent 0))
  "recursively prints a node and its children with indentation."
  (format t "~v@T~a~%" indent (car node)) ;; print the node's interval with indentation.
  (dolist (child (cdr node))
    (print-node child (+ indent 2)))) ;; increase indent for children.

(defun print-forest (forest)
  "prints all trees in the forest."
  (dolist (node forest)
    (print-node node 0)))

(defun mapcar-tree (node func)
  (cons (funcall func (car node))
        (mapcar (lambda (child)
                  (mapcar-tree child func))
                (cdr node))))

(defun mapcar-forest (forest func)
  "apply `mapcar-tree' to every node in the forest."
  (mapcar (lambda (node)
            (mapcar-tree node func))
          forest))

(defun map-tree (node func)
  (let ((result (mapcar (lambda (child)
                          (map-tree child func))
                        (cdr node))))
    (cons (funcall func node)
          result)))

(defun map-forest (forest func)
  "apply map-tree to every node in the forest."
  (mapcar (lambda (node)
            (map-tree node func))
          forest))

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