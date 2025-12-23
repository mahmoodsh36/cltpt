(defpackage :cltpt/combinator/match
  (:use :cl)
  (:import-from
   :cltpt/buffer
   :buffer :make-buffer :buffer-parent :buffer-children
   :buffer-region :buffer-region-begin :buffer-region-end
   :buffer-begin-absolute :buffer-end-absolute
   :make-region :region-begin :region-end)
  (:export
   :match-id :match-begin :match-end :match-ctx :match-children
   :match-begin-absolute :match-end-absolute
   :make-match :match-clone :match-rule :match-parent
   :match-set-children-parent :match-props :match :match-region
   :find-submatch :find-submatch-last :find-submatch-all
   :match-text))

(in-package :cltpt/combinator/match)

(defclass match (buffer)
  ((ctx
    :initarg :ctx
    :accessor match-ctx
    :initform nil
    :documentation "the parsing context.")
   (id
    :initarg :id
    :accessor match-id
    :initform nil
    :documentation "identifier for this match.")
   (props
    :initarg :props
    :accessor match-props
    :initform nil
    :documentation "additional properties.")
   (rule
    :initarg :rule
    :accessor match-rule
    :initform nil
    :documentation "the rule that produced this match."))
  (:documentation "a match represents a parsed region. inherits from buffer for parent/children/region."))

(defmethod print-object ((m match) stream)
  (print-unreadable-object (m stream :type t)
    (format stream "[~A:~A]~@[ id:~A~]"
            (match-begin m)
            (match-end m)
            (match-id m))))

;; some wrappers for cltpt/buffer/match functionality.
(defun match-begin (match)
  (when (and match (buffer-region match))
    (buffer-region-begin match)))

(defun (setf match-begin) (val match)
  (setf (buffer-region-begin match) val))

(defun match-end (match)
  (when (and match (buffer-region match))
    (buffer-region-end match)))

(defun (setf match-end) (val match)
  (setf (buffer-region-end match) val))

(defun match-children (match)
  (buffer-children match))

(defun (setf match-children) (val match)
  (setf (buffer-children match) val))

(defun match-parent (match)
  (buffer-parent match))

(defun (setf match-parent) (val match)
  (setf (buffer-parent match) val))

(defun match-region (match)
  (buffer-region match))

(defmethod match-begin-absolute ((subtree match))
  (buffer-begin-absolute subtree))

(defmethod match-end-absolute ((subtree match))
  (buffer-end-absolute subtree))

(defun make-match (&key begin end ctx id props rule children parent region)
  "create a match. if REGION is provided, its used directly. otherwise a region is created from BEGIN/END."
  (let ((m (make-instance 'match
                          :ctx ctx
                          :id id
                          :props props
                          :rule rule
                          :region (or region
                                      (make-region :begin (or begin 0)
                                                   :end (or end 0)))
                          :parent parent
                          :children children)))
    ;; set up parent-child relationships
    (when parent
      (push m (buffer-children parent)))
    m))

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
   :props (match-props match)
   :rule (match-rule match)
   :parent nil
   :children (mapcar 'match-clone (match-children match))))

;; implement cltpt/tree interface

(defmethod cltpt/tree:tree-children ((subtree match))
  (match-children subtree))

(defmethod cltpt/tree:tree-parent ((subtree match))
  (match-parent subtree))

(defmethod cltpt/tree:tree-value ((subtree match))
  subtree)

(defmethod match-text ((match match) input)
  "extract the text for MATCH from INPUT (either a string or a reader)."
  (when match
    (let ((begin (match-begin-absolute match))
          (end (match-end-absolute match)))
      (subseq input begin end))))