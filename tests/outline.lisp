(defpackage :cltpt/tests/outline
  (:use :cl :it.bese.fiveam)
  (:export :run-outline-tests))

(in-package :cltpt/tests/outline)

(def-suite outline-suite
  :description "tests for tree outline rendering."
  :in cltpt/tests::cltpt-suite)

(in-suite outline-suite)

(defparameter *test-forest*
  (list (cons '(:text "projects" :expanded t)
              (list (cons '(:text "lisp" :expanded t)
                          (list '(:text "cltpt")))
                    '(:text "python")))
        (cons '(:text "documents" :expanded t)
              (list '(:text "resume.docx")))
        '(:text "downloads")))

(defun test-outline-1 ()
  (format t "~%--- 1. lsblk style ---~%~a"
          (cltpt/tree/outline:render-outline *test-forest*))
  (format t "~%--- 2. ascii style ---~%~a"
          (cltpt/tree/outline:render-outline *test-forest* cltpt/tree/outline:*ascii-style*))
  (format t "~%--- 3. simple style ---~%~a"
          (cltpt/tree/outline:render-outline *test-forest* cltpt/tree/outline:*simple-style*)))

(defun test-outline-2 ()
  (format t "~%--- 1. s-expression output ---~%~a~%"
          (cltpt/tree/outline:render-as-s-expression *test-forest*))
  (format t "~%--- 2. json output ---~%~a~%"
          (cltpt/tree/outline:render-as-json *test-forest*))
  (format t "~%--- 3. graphviz output ---~%")
  (format t "save this as 'tree.dot' and run: dot -Tpng tree.dot -o tree.png~%~a"
          (cltpt/tree/outline:render-as-dot *test-forest*))
  (format t "~%--- 4. path list output ---~%~a"
          (cltpt/tree/outline:render-as-path-list *test-forest*)))

(test test-simple-style-nested
  (let* ((forest (list (cons '(:text "root" :expanded t)
                             (list (cons '(:text "level1" :expanded t)
                                         (list (cons '(:text "level2" :expanded t)
                                                     (list '(:text "level3")))
                                               '(:text "level2b")))
                                   '(:text "sibling")))))
         (result (cltpt/tree/outline:render-outline forest cltpt/tree/outline:*simple-style*)))
    (is (string= result
                 "- root
  - level1
    - level2
      - level3
    - level2b
  - sibling
"))))

(defun run-outline-tests ()
  (format t "~&running outline tests...~%")
  (let ((results (run! 'outline-suite)))
    (unless results
      (explain! results))))