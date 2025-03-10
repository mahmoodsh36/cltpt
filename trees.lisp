(defpackage :cltpt
  (:use :cl))
(in-package :cltpt)

(defun make-node (interval)
  "create a tree node from an interval.
the node is represented as a cons cell: (interval . children),
where interval is a list (start end id) and children is a list of nodes."
  (cons interval nil))

;; todo: optimize
(defun build-tree (intervals)
  "Build a nesting tree from a list of intervals.
Each interval is a list of the form (start end id).
Assumes intervals are strictly nested and sorted by start."
  (let ((forest nil)
        (stack nil))
    ;; Sort intervals by the start value.
    (setq intervals (sort intervals (lambda (a b)
                                       (< (first a) (first b)))))
    (dolist (interval intervals)
      (let ((node (make-node interval)))
        ;; While there is a node on the stack that does NOT enclose the current interval,
        ;; pop it off the stack.
        (loop while (and stack
                         (let* ((parent (first stack))
                                (pinterval (car parent))
                                (pstart (first pinterval))
                                (pend (second pinterval)))
                           (not (and (<= pstart (first interval))
                                     (>= pend (second interval))))))
              do (pop stack))
        (if stack
            ;; If the stack is non-empty, the top is a valid parent.
            (push node (cdr (first stack)))
            ;; Otherwise, this node is a top-level interval.
            (push node forest))
        ;; Push the current node onto the stack.
        (push node stack)))
    ;; Reverse forest to preserve original order.
    (nreverse forest)))

;; example usage
(let ((tree (build-tree '((0 29 id1)
                          (2 27 id2)
                          (10 20 id3)
                          (13 17 id4)
                          (22 25 id5)))))
  (format t "~%Tree: ~a~%" tree))

(defun print-node (node &optional (indent 0))
  "recursively prints a node and its children with indentation."
  (format t "~v@T~a~%" indent (car node)) ;; print the node's interval with indentation.
  (dolist (child (cdr node))
    (print-node child (+ indent 2)))) ; increase indent for children.

(defun print-forest (forest)
  "prints all trees in the forest."
  (dolist (node forest)
    (print-node node 0)))

;; example usage
(let ((tree (build-tree '((0 29 id1)
                          (2 27 id2)
                          (10 20 id3)
                          (13 17 id4)
                          (22 25 id5)))))
  (print-forest tree))

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
  (cons (funcall func node)
        (mapcar (lambda (child)
                  (map-tree child func))
                (cdr node))))

(defun map-forest (forest func)
  "apply map-tree to every node in the forest."
  (mapcar (lambda (node)
            (map-tree node func))
          forest))