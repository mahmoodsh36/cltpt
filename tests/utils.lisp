(defpackage :cltpt/tests/utils
  (:use :cl)
  (:export
   :plist-to-match
   :string=+diff
   :org-rules
   :rules-from-symbols
   :compare-tree-types
   :match-tree-equal-p
   :match-trees-equal-p
   :match-tree-diff
   :is-match-tree
   :is-match-trees))

(in-package :cltpt/tests/utils)

(defun match-tree-equal-p (actual expected)
  "recursively compare an actual match tree against an expected plist tree.
EXPECTED format: ((:ID id :BEGIN begin :END end) child1 child2 ...)
where each child has the same format.
returns T if trees match, NIL otherwise."
  (when (and (null actual) (null expected))
    (return-from match-tree-equal-p t))
  (when (or (null actual) (null expected))
    (return-from match-tree-equal-p nil))
  (let* ((expected-info (car expected))
         (expected-children (cdr expected))
         (expected-id (getf expected-info :id))
         (expected-begin (getf expected-info :begin))
         (expected-end (getf expected-info :end)))
    (unless (and (eql (cltpt/combinator/match:match-begin actual) expected-begin)
                 (eql (cltpt/combinator/match:match-end actual) expected-end)
                 (or (null expected-id)
                     (eql (cltpt/combinator/match:match-id actual) expected-id)))
      (return-from match-tree-equal-p nil))
    (unless (= (length (cltpt/combinator/match:match-children actual))
               (length expected-children))
      (return-from match-tree-equal-p nil))
    (loop for actual-child in (cltpt/combinator/match:match-children actual)
          for expected-child in expected-children
          always (match-tree-equal-p actual-child expected-child))))

(defun match-trees-equal-p (actual-list expected-list)
  "compare a list of match trees against a list of expected plist trees.
returns T if all trees match, NIL otherwise."
  (and (= (length actual-list) (length expected-list))
       (loop for actual in actual-list
             for expected in expected-list
             always (match-tree-equal-p actual expected))))

(defun match-tree-diff (actual expected &optional (path "root"))
  "return a string describing the first mismatch between actual match tree and expected plist tree, or NIL if they are equal."
  (cond
    ((and (null actual) (null expected))
     nil)
    ((or (null actual) (null expected))
     (format nil
             "at ~A: one side is nil~%  actual:   ~S~%  expected: ~S"
             path
             actual
             expected))
    (t
     (let* ((expected-info (car expected))
            (expected-children (cdr expected))
            (expected-id (getf expected-info :id))
            (expected-begin (getf expected-info :begin))
            (expected-end (getf expected-info :end))
            (actual-begin (cltpt/combinator/match:match-begin actual))
            (actual-end (cltpt/combinator/match:match-end actual))
            (actual-id (cltpt/combinator/match:match-id actual)))
       (cond
         ((not (and (eql actual-begin expected-begin)
                    (eql actual-end expected-end)
                    (or (null expected-id) (eql actual-id expected-id))))
          (format nil
                  "at ~A:~%  actual:   [~A:~A] id=~S~%  expected: [~A:~A] id=~S"
                  path
                  actual-begin
                  actual-end
                  actual-id
                  expected-begin
                  expected-end
                  expected-id))
         ((/= (length (cltpt/combinator/match:match-children actual))
              (length expected-children))
          (format nil
                  "at ~A: child count differs~%  actual:   ~A children~%  expected: ~A children"
                  path
                  (length (cltpt/combinator/match:match-children actual))
                  (length expected-children)))
         (t
          (loop for actual-child in (cltpt/combinator/match:match-children actual)
                for expected-child in expected-children
                for i from 0
                thereis (match-tree-diff
                         actual-child
                         expected-child
                         (format nil "~A[~A]" path i)))))))))

(defmacro is-match-tree (actual expected)
  "compare actual match tree against expected plist tree, reporting a helpful message on failure."
  (let ((a (gensym "ACTUAL"))
        (d (gensym "DIFF")))
    `(let* ((,a ,actual)
            (,d (match-tree-diff ,a ,expected)))
       (if ,d
           (it.bese.fiveam:fail "~A" ,d)
           (it.bese.fiveam:pass)))))

(defmacro is-match-trees (actual-list expected-list)
  "compare list of match trees against list of expected plist trees."
  (let ((al (gensym "ACTUAL-LIST"))
        (el (gensym "EXPECTED-LIST")))
    `(let ((,al ,actual-list)
           (,el ,expected-list))
       (if (/= (length ,al) (length ,el))
           (it.bese.fiveam:fail "list length differs: actual ~A, expected ~A"
                                (length ,al)
                                (length ,el))
           (loop for actual in ,al
                 for expected in ,el
                 for i from 0
                 do (let ((diff (match-tree-diff actual expected (format nil "tree[~A]" i))))
                      (when diff
                        (it.bese.fiveam:fail "~A" diff)
                        (return)))
                 finally (it.bese.fiveam:pass))))))

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
              (push (format nil
                            "~a: expected node of type ~a, but got nil"
                            current-path
                            (cltpt/tree:tree-value expected-node))
                    errors))
             ((null expected-node)
              (push (format nil
                            "~a: got unexpected node of type ~a"
                            current-path
                            (type-of actual-node))
                    errors))
             (t
              (let ((actual-type (type-of actual-node))
                    (actual-children (safe-tree-children actual-node))
                    (expected-type (cltpt/tree:tree-value expected-node))
                    (expected-children (safe-tree-children expected-node)))
                (unless (if (symbolp expected-type)
                            (equal actual-type expected-type)
                            (string= (symbol-name actual-type) expected-type))
                  (push (format nil
                                "~a: expected type ~a, got ~a"
                                current-path
                                expected-type
                                actual-type)
                        errors))
                ;; if expected-children is :IGNORE, skip children comparison entirely
                (unless (eq expected-children :ignore)
                  (unless (= (length actual-children) (length expected-children))
                    (push (format nil
                                  "~a: expected ~d children, got ~d"
                                  current-path
                                  (length expected-children)
                                  (length actual-children))
                          errors))
                  (loop for actual-child in actual-children
                        for expected-child in expected-children
                        for i from 0
                        do (compare-nodes actual-child
                                          expected-child
                                          (format nil "~a[~d]" current-path i)))))))))
      (compare-nodes actual-tree expected-types-tree path))
    (nreverse errors)))