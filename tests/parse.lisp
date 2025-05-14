(defpackage :cltpt-tests
  (:use :cl :it.bese.fiveam :cltpt/org-mode)
  (:import-from :cltpt/org-mode
                :org-list-parse
                :org-list-get-bounds)
  (:export #:run-cltpt-tests))

(in-package :cltpt-tests)

(def-suite cltpt-suite
  :description "tests for cltpt.")

(in-suite cltpt-suite)

(test org-list-parse-no-list
  (is (null (org-list-parse "this is not a list.")))
  (is (null (org-list-parse "")))
  (is (null (org-list-parse "  ")))
  (is (null (org-list-parse "leading text then nothing that is a list."))))

(test org-list-parse-basic
  (is (equalp (org-list-parse "- item 1
- item 2")
              '((:MARKER "-" :TEXT "item 1" :CHILDREN NIL)
                (:MARKER "-" :TEXT "item 2" :CHILDREN NIL))))
  (is (equalp (org-list-parse "1. first
   some extra
2. second")
              '((:MARKER "1." :TEXT "first
some extra" :CHILDREN NIL)
                (:MARKER "2." :TEXT "second" :CHILDREN NIL)))))

(test org-list-parse-nested
  (let ((text "- item one
   extra text for one
- item two
   a. nested item one
      more nested text
      i. even more nested
   b. nested item two
- item three"))
    (is (equalp (org-list-parse text)
                '((:MARKER "-" :TEXT "item one
extra text for one" :CHILDREN NIL)
                  (:MARKER "-" :TEXT "item two" :CHILDREN
                   ((:MARKER "a." :TEXT "nested item one
more nested text" :CHILDREN
                     ((:MARKER "i." :TEXT "even more nested" :CHILDREN NIL)))
                    (:MARKER "b." :TEXT "nested item two" :CHILDREN NIL)))
                  (:MARKER "-" :TEXT "item three" :CHILDREN NIL))))))

(test org-list-get-bounds-basic
  (is (equal (org-list-get-bounds "- item 1
- item 2")
             (cons 0 17)))
  (is (equal (org-list-get-bounds "1. first
   some extra
2. second")
             (cons 0 32))))

(test org-list-get-bounds-with-preamble-and-trailing
  (is (equal (org-list-get-bounds "preamble.
- item 1
after.")
             (cons 10 18)))

  (is (equal (org-list-get-bounds "- item 1
trailing text")
             (cons 0 8))))

(test org-list-get-bounds-indented
  (is (equal (org-list-get-bounds "  - item")
             (cons 2 8)))
  (is (equal (org-list-get-bounds (format nil "  ~C~C  - properly indented list~C    more stuff for it." #\Newline #\Newline #\Newline))
             (cons 6 53))))

(test org-list-get-bounds-complex
  (let ((text "wew
- item one
   extra text for one
- item two
   a. nested item one
      more nested text
      i. even more nested
   b. nested item two
- item three
   another line for three
hey"))
    (is (equal (org-list-get-bounds text)
               (cons 4 (- (length text) 4))))))

(test org-list-get-bounds-no-list
  (is (equal (org-list-get-bounds "this is not a list.")
             (cons nil nil)))
  (is (equal (org-list-get-bounds "")
             (cons nil nil)))
  (is (equal (org-list-get-bounds "  ")
             (cons nil nil))))

(test org-list-get-bounds-case2
    (let ((text "preamble text.
  - list item 1 starts here
  - list item 2
    - nested
after list."))
      (is (equal (org-list-get-bounds text) (cons 17 71)))))

(defun run-cltpt-tests ()
  "runs all defined fiveam test suites for cltpt."
  (format t "~&running cltpt tests...~%")
  (let ((results (run! 'cltpt-suite)))
    (unless results
      (explain! results))))