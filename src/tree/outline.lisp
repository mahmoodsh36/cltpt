(defpackage :cltpt/outline
  (:use :cl)
  (:export
   :should-expand
   :outline-text
   :render-tree
   :render-forest
   :render-outline
   :*ascii-style* :*simple-style* :*lsblk-style*
   :render-as-s-expression
   :render-as-json
   :render-as-dot
   :render-as-path-list))

(in-package :cltpt/outline)

(defgeneric should-expand (node)
  (:documentation "whether to expand a node when displaying the outline."))

(defgeneric outline-text (node)
  (:documentation "the text to display for a node in the generated outline."))

;; (defmethod expand-node ((node cons))
;;   (setf (getf node :expanded) t))

;; (defmethod collapse-node ((node cons))
;;   (setf (getf node :expanded) nil))

(defmethod should-expand ((node cons))
  "Returns true only if the node has a branch structure AND the :expanded flag."
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
  (cltpt/tree:tree-value node))

(defclass drawing-style ()
  ((branch-connector :initarg :branch-connector :reader branch-connector)
   (last-branch-connector :initarg :last-branch-connector :reader last-branch-connector)
   (child-prefix :initarg :child-prefix :reader child-prefix)
   (last-child-prefix :initarg :last-child-prefix :reader last-child-prefix)))

(defparameter *lsblk-style*
  (make-instance 'drawing-style
                 :branch-connector "├─" :last-branch-connector "└─"
                 :child-prefix "│ " :last-child-prefix "  "))

(defparameter *ascii-style*
  (make-instance 'drawing-style
                 :branch-connector "+--" :last-branch-connector "\\--"
                 :child-prefix "|  " :last-child-prefix "   "))

(defun is-branch-p (node)
  "Reliably checks if a node is a branch (has children) based on its structure."
  (and (consp node)
       (consp (car node))
       (keywordp (caar node))))

(defparameter *simple-style*
  (make-instance 'drawing-style
                 :branch-connector " - " :last-branch-connector " - "
                 :child-prefix "   " :last-child-prefix "   "))

(defun render-outline (nodes &optional (style *lsblk-style*))
  "core function that renders a list of nodes using a specified drawing style."
  (with-output-to-string (out)
    (labels ((display-nodes (nodes-list prefix)
               (loop for node in nodes-list
                     for lastp = (eq node (car (last nodes-list)))
                     do (let* ((connector (if lastp
                                              (last-branch-connector style)
                                              (branch-connector style)))
                               (child-prefix (if lastp
                                                 (last-child-prefix style)
                                                 (child-prefix style))))
                          (format out
                                  "~a~a ~a~%"
                                  prefix
                                  connector
                                  (outline-text node))
                          (when (and (should-expand node)
                                     (cltpt/tree:tree-children node))
                            (display-nodes
                             (cltpt/tree:tree-children node)
                             (concatenate 'string prefix child-prefix)))))))
      (display-nodes nodes ""))))

(defun render-as-s-expression (nodes)
  (labels ((to-sexp (node)
             ;; TODO: should we be doing the structural check here?
             (if (and (consp node) (consp (car node)))
                 (cons (outline-text node)
                       (mapcar #'to-sexp (cltpt/tree:tree-children node)))
                 (outline-text node))))
    (format nil "~s" (mapcar #'to-sexp nodes))))

(defun render-as-json (nodes)
  (with-output-to-string (out)
    (labels ((to-json (node lastp)
               (format out "{\"text\": \"~a\"" (outline-text node))
               ;; TODO: should we be doing the structural check here?
               (when (and (consp node) (consp (car node)))
                 (format out ", \"children\": [")
                 (loop for child in (cltpt/tree:tree-children node)
                       for is-last = (eq child (car (last (cltpt/tree:tree-children node))))
                       do (to-json child is-last))
                 (format out "]"))
               (format out "}~a" (if lastp "" ","))))
      (format out "[")
      (loop for node in nodes
            for lastp = (eq node (car (last nodes)))
            do (to-json node lastp))
      (format out "]"))))

(defun render-as-dot (nodes)
  (let ((counter 0))
    (with-output-to-string (out)
      (format out "digraph G {~%  rankdir=TB;~%")
      (labels ((to-dot (node parent-id)
                 (let ((my-id (incf counter)) (my-text (outline-text node)))
                   (format out "  node~a [label=\"~a\"];~%" my-id my-text)
                   (when parent-id
                     (format out "  node~a -> node~a;~%" parent-id my-id))
                   ;; TODO: should we be doing the structural check here?
                   (when (and (consp node) (consp (car node)))
                     (loop for child in (cltpt/tree:tree-children node) do
                       (to-dot child my-id))))))
        (loop for node in nodes do (to-dot node nil)))
      (format out "}~%"))))

(defun render-as-path-list (nodes)
  (with-output-to-string (out)
    (labels ((to-path (node current-path)
               (let ((new-path
                       (format nil
                               "~a/~a"
                               current-path
                               (outline-text node))))
                 (format out "~a~%" (subseq new-path 1))
                 ;; TODO: should we be doing the structural check here?
                 (when (and (consp node) (consp (car node)))
                   (loop for child in (cltpt/tree:tree-children node) do
                     (to-path child new-path))))))
      (loop for node in nodes do (to-path node "")))))

(defun render-forest (nodes)
  "convenience wrapper to render a forest with the default lsblk style."
  (render-outline nodes *lsblk-style*))

(defun render-tree (root-node &optional (style *lsblk-style*))
  "renders a single tree with its root un-prefixed, using a given style."
  (with-output-to-string (out)
    (format out "~a~%" (outline-text root-node))
    (when (and (should-expand root-node) (cltpt/tree:tree-children root-node))
      (write-string (render-outline (cltpt/tree:tree-children root-node) style)
                    out))))