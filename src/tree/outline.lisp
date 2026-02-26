(defpackage :cltpt/tree/outline
  (:use :cl)
  (:export
   :should-expand
   :outline-text
   :render-tree
   :render-forest
   :render-outline
   :*lsblk-style* :*ascii-style* :*simple-style*
   :render-as-s-expression
   :render-as-json
   :render-as-dot
   :render-as-path-list
   :could-expand))

(in-package :cltpt/tree/outline)

(defgeneric should-expand (node)
  (:documentation "whether a node should be expanded by default."))

(defgeneric could-expand (node)
  (:documentation "whether a node can be expanded."))

(defmethod should-expand ((node cons))
  "returns true only if the node has a branch structure AND the :expanded flag."
  (and (consp (car node))
       (getf (car node) :expanded)))

(defmethod outline-text ((node cons))
  "correctly gets text from either a branch or a leaf plist."
  (if (consp (car node))
      (getf (car node) :text)
      (getf node :text)))

(defmethod should-expand ((node t))
  (cltpt/tree:tree-children node))

(defmethod outline-text ((node string))
  node)

(defmethod outline-text ((node t))
  (type-of (cltpt/tree:tree-value node)))

(defmethod could-expand ((node t))
  (should-expand node))

(defclass render-style () ()
  (:documentation "base class for all render styles."))

(defclass drawing-style (render-style)
  ((branch-connector :initarg :branch-connector :reader branch-connector)
   (last-branch-connector :initarg :last-branch-connector :reader last-branch-connector)
   (child-prefix :initarg :child-prefix :reader child-prefix)
   (last-child-prefix :initarg :last-child-prefix :reader last-child-prefix)))

(defclass json-style (render-style) ())
(defclass sexp-style (render-style) ())
(defclass dot-style (render-style) ())
(defclass path-style (render-style) ())
(defclass indented-json-style (render-style)
  ((indent :initarg :indent :reader indent :initform 2)))

(defparameter *lsblk-style*
  (make-instance 'drawing-style
                 :branch-connector "├─ " :last-branch-connector "└─ "
                 :child-prefix "│ " :last-child-prefix "  "))

(defparameter *ascii-style*
  (make-instance 'drawing-style
                 :branch-connector "+-- " :last-branch-connector "\\-- "
                 :child-prefix "|  " :last-child-prefix "   "))

(defparameter *simple-style*
  (make-instance 'drawing-style
                 :branch-connector "- " :last-branch-connector "- "
                 :child-prefix "  " :last-child-prefix "  "))

(defparameter *json-style* (make-instance 'json-style))
(defparameter *dot-style*  (make-instance 'dot-style))
(defparameter *sexp-style* (make-instance 'sexp-style))
(defparameter *path-style* (make-instance 'path-style))
(defparameter *indented-json-style* (make-instance 'indented-json-style :indent 2))

(defgeneric render-outline (nodes style)
  (:documentation "render a list of nodes using the given style."))

(defmethod render-outline (nodes (style drawing-style))
  (with-output-to-string (out)
    (labels ((render-nodes (nodes-list prefix)
               (loop for rest on nodes-list
                     for node = (car rest)
                     for lastp = (null (cdr rest))
                     for children = (cltpt/tree:tree-children node)
                     do (let ((connector (if lastp
                                             (last-branch-connector style)
                                             (branch-connector style)))
                              (child-pfx (if lastp
                                             (last-child-prefix style)
                                             (child-prefix style))))
                          (format out
                                  "~a~a~a~%"
                                  prefix
                                  connector
                                  (outline-text node))
                          (when (and (should-expand node) children)
                            (render-nodes children
                                          (concatenate 'string prefix child-pfx)))))))
      (render-nodes nodes ""))))

(defmethod render-outline (nodes (style path-style))
  (with-output-to-string (out)
    (labels ((render-nodes (nodes-list prefix)
               (loop for node in nodes-list
                     for new-path = (if (string= prefix "")
                                        (outline-text node)
                                        (format nil "~a/~a" prefix (outline-text node)))
                     do (format out "~a~%" new-path)
                        (when (should-expand node)
                          (render-nodes (cltpt/tree:tree-children node) new-path)))))
      (render-nodes nodes ""))))

(defmethod render-outline (nodes (style sexp-style))
  (labels ((to-sexp (node)
             (let ((text (outline-text node))
                   (children (cltpt/tree:tree-children node)))
               (if (and (should-expand node) children)
                   (cons text (mapcar #'to-sexp children))
                   text))))
    (format nil "(~{~a~^ ~})" (mapcar #'to-sexp nodes))))

(defmethod render-outline (nodes (style json-style))
  (labels ((to-json (node)
             (let ((text (outline-text node))
                   (children (cltpt/tree:tree-children node)))
               (if (and (should-expand node) children)
                   (format nil
                           "{\"text\":\"~a\",\"children\":[~{~a~^,~}]}"
                           text
                           (mapcar #'to-json children))
                   (format nil "{\"text\":\"~a\"}" text)))))
    (format nil "[~{~a~^,~}]" (mapcar #'to-json nodes))))

(defmethod render-outline (nodes (style indented-json-style))
  (with-output-to-string (out)
    (labels ((to-json (node indent)
               (let* ((n (indent style))
                      (text (outline-text node))
                      (children (cltpt/tree:tree-children node))
                      (indent-str (make-string indent :initial-element #\space))
                      (prop-indent (make-string (+ indent n) :initial-element #\space)))
                 (format out "~a{~%" indent-str)
                 (format out "~a\"text\": \"~a\"" prop-indent text)
                 (when (and (should-expand node) children)
                   (write-string "," out)
                   (format out "~%~a\"children\": [~%" prop-indent)
                   (loop for child in children
                         for i from 0
                         for child-count = (length children)
                         do (to-json child (+ indent n n))
                            (unless (= (1+ i) child-count)
                              (write-string "," out))
                            (format out "~%"))
                   (format out "~a]" (make-string (+ indent n) :initial-element #\space)))
                 (format out "~%~a}" indent-str))))
      (format out "[~%")
      (loop for node in nodes
            for i from 0
            for node-count = (length nodes)
            do (to-json node (indent style))
               (unless (= (1+ i) node-count)
                 (write-string "," out))
               (format out "~%"))
      (write-string "]" out))))

(defmethod render-outline (nodes (style dot-style))
  (let ((counter 0))
    (with-output-to-string (out)
      (format out "digraph G {~%  rankdir=TB;~%")
      (labels ((to-dot (node parent-id)
                 (let ((my-id (incf counter))
                       (my-text (outline-text node)))
                   (format out "  node~a [label=\"~a\"];~%" my-id my-text)
                   (when parent-id
                     (format out "  node~a -> node~a;~%" parent-id my-id))
                   (when (should-expand node)
                     (loop for child in (cltpt/tree:tree-children node)
                           do (to-dot child my-id))))))
        (loop for node in nodes do (to-dot node nil)))
      (format out "}~%"))))

(defun render-as-s-expression (nodes) (render-outline nodes *sexp-style*))
(defun render-as-json (nodes)         (render-outline nodes *json-style*))
(defun render-as-dot (nodes)          (render-outline nodes *dot-style*))
(defun render-as-path-list (nodes)    (render-outline nodes *path-style*))

(defun render-forest (nodes)
  "convenience wrapper to render a forest with the default lsblk style."
  (render-outline nodes *lsblk-style*))

(defgeneric render-tree (root-node style)
  (:documentation "render a single tree rooted at ROOT-NODE using STYLE."))

(defmethod render-tree (root-node (style drawing-style))
  "renders the root un-prefixed, then its children with the drawing style."
  (with-output-to-string (out)
    (format out "~a~%" (outline-text root-node))
    (when (and (should-expand root-node) (cltpt/tree:tree-children root-node))
      (write-string (render-outline (cltpt/tree:tree-children root-node) style)
                    out))))

(defmethod render-tree (root-node (style render-style))
  "default: render the tree as a single-element forest."
  (render-outline (list root-node) style))
