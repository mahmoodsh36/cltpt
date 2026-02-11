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

;; a match represents a parsed region. inherits from buffer for parent/children/region.
(defstruct (match (:print-function print-match)
                  (:constructor %make-match))
  ;; the parsing context
  (ctx nil :type t)
  ;; identifier for this match
  (id nil :type symbol)
  ;; additional properties
  (props nil :type list)
  ;; the rule that produced this match.
  (rule nil :type t)
  (begin 0 :type fixnum)
  (end 0 :type fixnum)
  (children nil :type list)
  (parent nil :type (or null match)))

(defun print-match (m stream)
  (print-unreadable-object (m stream :type t)
    (format stream "[~A:~A]~@[ id:~A~]"
            (match-begin m)
            (match-end m)
            (match-id m))))

(defun match-region (match)
  (make-region :begin (match-begin match)
               :end (match-end match)))

(defmethod match-begin-absolute ((subtree match))
  ;; assuming relative for now as buffer inheritance is gone,
  ;; but logic needs to be verified if absolute positioning relied on buffer traversal
  (let ((pos (match-begin subtree))
        (m subtree))
    (loop for parent = (match-parent m)
          while parent
          do (incf pos (match-begin parent))
             (setf m parent))
    pos))

(defmethod match-end-absolute ((subtree match))
  (let ((pos (match-end subtree))
        (m subtree))
    (loop for parent = (match-parent m)
          while parent
          do (incf pos (match-begin parent))
             (setf m parent))
    pos))

(defun make-match (&key begin end ctx id props rule children parent region)
  "create a match. if REGION is provided, its used directly. otherwise a region is created from BEGIN/END."
  (let* ((b (if region (cltpt/buffer:region-begin region) (or begin 0)))
         (e (if region (cltpt/buffer:region-end region) (or end 0)))
         (m (%make-match
             :ctx ctx
             :id id
             :props props
             :rule rule
             :begin b
             :end e
             :parent parent
             :children children)))
    ;; set up parent-child relationships
    (when parent
      (push m (match-children parent)))
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

;; implement cltpt/buffer interface (duck typing for match struct)

(defmethod cltpt/buffer:buffer-children ((m match))
  (match-children m))

(defmethod (setf cltpt/buffer:buffer-children) (val (m match))
  (setf (match-children m) val))

(defmethod cltpt/buffer:buffer-parent ((m match))
  (match-parent m))

(defmethod (setf cltpt/buffer:buffer-parent) (val (m match))
  (setf (match-parent m) val))

(defmethod cltpt/buffer:buffer-region-begin ((m match))
  (match-begin m))

(defmethod (setf cltpt/buffer:buffer-region-begin) (val (m match))
  (setf (match-begin m) val))

(defmethod cltpt/buffer:buffer-region-end ((m match))
  (match-end m))

(defmethod (setf cltpt/buffer:buffer-region-end) (val (m match))
  (setf (match-end m) val))

(defmethod cltpt/buffer:buffer-region-length ((m match))
  (- (match-end m) (match-begin m)))

;; reimplement buffer absolute positioning logic for match struct
(defmethod cltpt/buffer:buffer-begin-absolute ((m match))
  (match-begin-absolute m))

(defmethod cltpt/buffer:buffer-end-absolute ((m match))
  (match-end-absolute m))

(defmethod cltpt/buffer:buffer-text ((m match))
  (warn "buffer-text called on match struct but match doesn't store text directly.")
  nil)

(defmethod match-text ((match match) input)
  "extract the text for MATCH from INPUT (either a string or a reader)."
  (when match
    (let ((begin (match-begin-absolute match))
          (end (match-end-absolute match)))
      (subseq input begin end))))