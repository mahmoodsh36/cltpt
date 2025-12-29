(defpackage :cltpt/tests/utils
  (:use :cl)
  (:export
   :simplify-match
   :compare-match-loosely
   :compare-full-match-loosely
   :plist-to-match
   :string=+diff
   :org-rules
   :rules-from-symbols
   :compare-tree-types))

(in-package :cltpt/tests/utils)

(defun simplify-match (match)
  "simplify a match to a plist with :begin and :end keys for comparison."
  (let ((new-match))
    (when (listp match)
      (when (getf match :begin)
        (setf (getf new-match :begin) (getf match :begin)))
      (when (getf match :end)
        (setf (getf new-match :end) (getf match :end)))
      (let ((str (getf match :str))
            (begin (getf match :begin))
            (end (getf match :end)))
        (when (and str begin end)
          (setf (getf new-match :match) (subseq str begin end)))))
    (when (typep match 'cltpt/combinator/match::match)
      (setf (getf new-match :begin)
            (cltpt/combinator:match-begin match))
      (setf (getf new-match :end)
            (cltpt/combinator:match-end match)))))

(defun compare-match-loosely (match1 match2)
  (let ((match11 (simplify-match match1))
        (match22 (simplify-match match2)))
    (equalp match11 match22)))

(defun compare-full-match-loosely (match1 match2)
  (cltpt/tree::trees-map
   (list match1 match2)
   (lambda (submatch1 submatch2)
     (compare-match-loosely submatch1
                            (car submatch2)))))

(defun plist-to-match (plist-tree)
  "convert a plist-based parse tree to match structs."
  (when plist-tree
    (let ((parent-info (car plist-tree))
          (children (cdr plist-tree)))
      (cltpt/combinator/match::make-match
       :id (getf parent-info :id)
       :begin (getf parent-info :begin)
       :end (getf parent-info :end)
       :ctx nil
       :children (mapcar #'plist-to-match children)))))

(defun string=+diff (actual expected &optional (test-name "String comparison"))
  "compare two strings and show diff if they are not equal, returning T if equal."
  (if (string= actual expected)
      t
      (progn
        (format t "~%~a: FAILED - strings do not match~%" test-name)
        (let ((expected-file (format nil "/tmp/cltpt-expected-~a-tmp" (get-universal-time)))
              (actual-file (format nil "/tmp/cltpt-actual-~a-tmp" (get-universal-time))))
          (with-open-file (stream expected-file :direction :output :if-exists :supersede)
            (write-string expected stream))
          (with-open-file (stream actual-file :direction :output :if-exists :supersede)
            (write-string actual stream))
          (format t "~%~%diff output:~%")
          (uiop:run-program (format nil "diff -u ~a ~a" expected-file actual-file)
                            :output *standard-output*
                            :error-output *error-output*
                            :ignore-error-status t)
          (uiop:delete-file-if-exists (pathname expected-file))
          (uiop:delete-file-if-exists (pathname actual-file))
          (format t "~%~%")
          nil))))

(defun org-rules ()
  (remove-if-not
   'identity
   (loop
     for type1
       in (cltpt/base:text-format-text-object-types cltpt/org-mode:*org-mode*)
     collect (cltpt/base:text-object-rule-from-subclass type1))))

(defun rules-from-symbols (syms)
  (remove-if-not
   'identity
   (loop for sym in syms
         collect (cltpt/base:text-object-rule-from-subclass sym))))

(defun compare-tree-types (actual-tree expected-types-tree &optional (path "root"))
  "iterate through a text-object tree and validate text-object-types.

ACTUAL-TREE is the parsed text-object tree.
EXPECTED-TYPES-TREE is a cons tree of type symbols (car is the type, cdr is children list).

when expected children is :IGNORE, skip children comparison for that node.

returns a list of error messages if types don't match, NIL if all match."
  (let ((errors))
    (labels
        ((safe-tree-children (node)
           (handler-case
               (cltpt/tree:tree-children node)
             (error ()
               nil)))
         (compare-nodes (actual-node expected-node current-path)
           (cond
             ((and (null actual-node) (null expected-node))
              nil)
             ((null actual-node)
              (push (format nil "~a: expected node of type ~a, but got nil"
                            current-path (cltpt/tree:tree-value expected-node))
                    errors))
             ((null expected-node)
              (push (format nil "~a: got unexpected node of type ~a"
                            current-path (type-of actual-node))
                    errors))
             (t
              (let ((actual-type (type-of actual-node))
                    (actual-children (safe-tree-children actual-node))
                    (expected-type (cltpt/tree:tree-value expected-node))
                    (expected-children (safe-tree-children expected-node)))
                (unless (if (symbolp expected-type)
                            (equal actual-type expected-type)
                            (string= (symbol-name actual-type) expected-type))
                  (push (format nil "~a: expected type ~a, got ~a"
                                current-path expected-type actual-type)
                        errors))
                ;; if expected-children is :IGNORE, skip children comparison entirely
                (unless (eq expected-children :ignore)
                  (unless (= (length actual-children) (length expected-children))
                    (push (format nil "~a: expected ~d children, got ~d"
                                  current-path (length expected-children) (length actual-children))
                          errors))
                  (loop for actual-child in actual-children
                        for expected-child in expected-children
                        for i from 0
                        do (compare-nodes actual-child expected-child
                                          (format nil "~a[~d]" current-path i)))))))))
      (compare-nodes actual-tree expected-types-tree path))
    (nreverse errors)))