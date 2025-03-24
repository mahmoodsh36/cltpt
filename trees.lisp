(in-package :cltpt)

(defun make-node (interval)
  (cons interval nil))

;; todo: optimize
(defun build-tree (intervals)
  "build a nesting tree from a list of intervals.
each interval is a list of the form (start end id).
assumes intervals are strictly nested and sorted by start."
  (let ((forest)
        (stack))
    ;; sort intervals by the start value.
    (setq intervals
          (sort intervals (lambda (a b)
                            (< (first a) (first b)))))
    (dolist (interval intervals)
      (let ((node (make-node interval)))
        ;; while there is a node on the stack that does NOT enclose the current interval,
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
            ;; if the stack is non-empty, the top is a valid parent.
            (push node (cdr (first stack)))
            ;; otherwise, this node is a top-level interval.
            (push node forest))
        ;; push the current node onto the stack.
        (push node stack)))
    ;; reverse forest to preserve original order.
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
  (cons (funcall func node)
        (mapcar (lambda (child)
                  (map-tree child func))
                (cdr node))))

(defun map-forest (forest func)
  "apply map-tree to every node in the forest."
  (mapcar (lambda (node)
            (map-tree node func))
          forest))