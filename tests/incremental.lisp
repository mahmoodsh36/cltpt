(defpackage :cltpt/tests/incremental
  (:use :cl :it.bese.fiveam)
  (:export :run-incremental-tests))

(in-package :cltpt/tests/incremental)

(def-suite incremental-suite
  :description "tests for incremental parsing functionality."
  :in cltpt/tests::cltpt-suite)

(in-suite incremental-suite)

;; helper function to apply a change with reparsing
(defun apply-change-with-reparse (obj format start end new-text)
  "schedule and apply a change with reparsing enabled."
  (setf (cltpt/buffer:buffer-own-text obj) (cltpt/base:text-object-text obj))
  (cltpt/buffer:schedule-change*
   obj
   (cltpt/buffer:make-change
    :region (cltpt/buffer:make-region :begin start :end end)
    :operator new-text
    :args '(:delegate nil :reparse t)))
  (cltpt/buffer:apply-scheduled-changes
   obj
   :on-apply (cltpt/base:make-reparse-callback obj format)))

;; test for the buffer.lisp-based reparsing using make-reparse-callback
(defun test-buffer-reparse-callback ()
  "test that apply-scheduled-changes with make-reparse-callback produces correct reparsing."
  (let* ((text "start \\(math here\\) here")
         (format (cltpt/base:make-text-format "dummy" (list 'cltpt/latex::inline-math)))
         (doc (cltpt/base:parse format text))
         (initial-children-count (length (cltpt/base:text-object-children doc))))
    (format t "num of children before: ~A~%" initial-children-count)
    (format t "initial text: ~A~%" (cltpt/base:text-object-text doc))
    (apply-change-with-reparse doc format 6 6 "\\(new math\\) ")
    (format t "text after: ~A~%" (cltpt/base:text-object-text doc))
    (format t "children after: ~A~%" (cltpt/base:text-object-children doc))
    (let ((final-children-count (length (cltpt/base:text-object-children doc))))
      (format t "num of children after: ~A~%~%" final-children-count)
      (values initial-children-count final-children-count))))

(test test-buffer-reparse-callback
  "test that using make-reparse-callback produces correct reparsing results."
  (multiple-value-bind (initial-children-count final-children-count)
      (test-buffer-reparse-callback)
    (fiveam:is (= initial-children-count 1)
               "document should have 1 child before the operation (inline-math)")
    (fiveam:is (= final-children-count 2)
               "document should have 2 children after the operation (two inline-math)")))

(defun test-incremental-insertion ()
  (let* ((text "start \\(math here\\) end")
         (format (cltpt/base:make-text-format "dummy" (list 'cltpt/latex::inline-math)))
         (doc (cltpt/base:parse format text))
         (initial-children (length (cltpt/base:text-object-children doc))))
    (apply-change-with-reparse doc format 20 20 " \\(more math\\)")
    (let ((final-children (length (cltpt/base:text-object-children doc))))
      (values initial-children final-children))))

(test test-incremental-insertion
  (multiple-value-bind (initial-children final-children)
      (test-incremental-insertion)
    (fiveam:is (= initial-children 1)
               "should start with 1 child")
    (fiveam:is (= final-children 2)
               "should have 2 children after insertion")))

(defun test-incremental-replacement ()
  (let* ((text "test \\(old math\\) here")
         (format (cltpt/base:make-text-format "dummy" (list 'cltpt/latex::inline-math)))
         (doc (cltpt/base:parse format text))
         (initial-text (cltpt/base:text-object-text doc)))
    ;; replace "old" with "new"
    (apply-change-with-reparse doc format 7 10 "new")
    (let ((final-text (cltpt/base:text-object-text doc)))
      (values initial-text final-text))))

(test test-incremental-replacement
  (multiple-value-bind (initial-text final-text)
      (test-incremental-replacement)
    (fiveam:is (string= initial-text "test \\(old math\\) here"))
    (fiveam:is (string= final-text "test \\(new math\\) here"))))

(defun test-reparse-structure-ordering ()
  (let* ((text "#+title: test doc

* section one
some text with \\(math\\) here.

#+begin_src lisp
(+ 1 2)
#+end_src

** subsection A
content A.

* section two
more \\(inline math\\) text.")
         (format cltpt/org-mode:*org-mode*)
         (doc (cltpt/base:parse format text)))
    (let ((expected-initial-tree
            '(cltpt/org-mode::org-document
              (cltpt/org-mode::org-keyword)
              (cltpt/org-mode::org-header
               (cltpt/latex::inline-math)
               (cltpt/org-mode::org-src-block)
               (cltpt/org-mode::org-header))
              (cltpt/org-mode::org-header
               (cltpt/latex::inline-math)))))
      (let ((initial-errors (cltpt/tests/utils:compare-tree-types doc expected-initial-tree)))
        (format t "initial tree: ~A~%" (if initial-errors "MISMATCH" "MATCH"))
        (when initial-errors
          (format t "  errors: ~{~A~^, ~}~%" initial-errors))
        (let* ((insert-pos (+ (search "content A." text) 11))
               (new-content "
** subsection B
content with \\(more math\\).

"
                            ))
          (apply-change-with-reparse doc format insert-pos insert-pos new-content)
          (let ((expected-final-tree
                  '(cltpt/org-mode::org-document
                    (cltpt/org-mode::org-keyword)
                    (cltpt/org-mode::org-header
                     (cltpt/latex::inline-math)
                     (cltpt/org-mode::org-src-block)
                     (cltpt/org-mode::org-header)
                     (cltpt/org-mode::org-header
                      (cltpt/latex::inline-math)))
                    (cltpt/org-mode::org-header
                     (cltpt/latex::inline-math)))))
            (let ((final-errors (cltpt/tests/utils:compare-tree-types doc expected-final-tree)))
              (format t "final tree: ~A~%" (if final-errors "MISMATCH" "MATCH"))
              (when final-errors
                (format t "  errors: ~{~A~^, ~}~%" final-errors))
              (values (null initial-errors)
                      (null final-errors)))))))))

(test test-reparse-structure-ordering
  (multiple-value-bind (initial-ok final-ok)
      (test-reparse-structure-ordering)
    (fiveam:is-true initial-ok "initial tree should match expected")
    (fiveam:is-true final-ok "final tree should match expected after reparsing")))

(defun run-incremental-tests ()
  (format t "~&running incremental parsing tests...~%")
  (let ((results (run! 'incremental-suite)))
    (unless results
      (explain! results))))