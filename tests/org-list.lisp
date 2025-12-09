(defpackage :cltpt/tests/org-list
  (:use :cl :it.bese.fiveam)
  (:import-from :cltpt/tests
                :compare-full-match-loosely)
  (:export #:run-org-list-tests))

(in-package :cltpt/tests/org-list)

(def-suite org-list-suite
  :description "tests for org-list functionality."
  :in cltpt/tests::cltpt-suite)

(in-suite org-list-suite)

(defun org-list-test-1-func ()
  (let ((text "- we have \\(x=y\\)
   a. nested item one
      more nested text
   b. nested item two
- item three"))
    (cltpt/org-mode::org-list-matcher
     nil
     (cltpt/reader:reader-from-string text)
     0
     '((:pattern (cltpt/combinator::pair
                  (cltpt/combinator::literal "\\(")
                  (cltpt/combinator::literal "\\)")
                  nil)
        :id mypair)))))

(test org-list-test-1
  (is
   (compare-full-match-loosely
    (org-list-test-1-func)
    '((:ID CLTPT/ORG-MODE:ORG-LIST :BEGIN 0 :END 97 :INDENT 0)
      ((:ID CLTPT/ORG-MODE::LIST-ITEM :INDENT 0 :BEGIN 0 :END 85)
       ((:ID CLTPT/ORG-MODE::LIST-ITEM-BULLET :BEGIN 0 :END 2))
       ((:ID CLTPT/ORG-MODE::LIST-ITEM-CONTENT :BEGIN 2 :END 18)
        ((:BEGIN 10 :END 17 :ID MYPAIR)
         ((:BEGIN 10 :END 12)) ((:BEGIN 15 :END 17)))
        ((:ID CLTPT/ORG-MODE:ORG-LIST :BEGIN 18 :END 85 :INDENT 3)
         ((:ID CLTPT/ORG-MODE::LIST-ITEM :INDENT 3 :BEGIN 18 :END 63)
          ((:ID CLTPT/ORG-MODE::LIST-ITEM-BULLET :BEGIN 21 :END 24))
          ((:ID CLTPT/ORG-MODE::LIST-ITEM-CONTENT :BEGIN 24 :END 63)))
         ((:ID CLTPT/ORG-MODE::LIST-ITEM :INDENT 3 :BEGIN 63 :END 85)
          ((:ID CLTPT/ORG-MODE::LIST-ITEM-BULLET :BEGIN 66 :END 69))
          ((:ID CLTPT/ORG-MODE::LIST-ITEM-CONTENT :BEGIN 69 :END 85))))))
      ((:ID CLTPT/ORG-MODE::LIST-ITEM :INDENT 0 :BEGIN 85 :END 97)
       ((:ID CLTPT/ORG-MODE::LIST-ITEM-BULLET :BEGIN 85 :END 87))
       ((:ID CLTPT/ORG-MODE::LIST-ITEM-CONTENT :BEGIN 87 :END 97)))))))

(defun org-list-test-2-func ()
  (let ((text "- item one
  extra text for one
- we have \\(x=y\\)
  a. nested item one
     more nested text
     i. even more nested
  b. nested item two
- item three"))
    (cltpt/org-mode::org-list-matcher
     nil
     (cltpt/reader:reader-from-string text)
     0
     '((:pattern (cltpt/combinator::literal "item") :id item-keyword)
       (:pattern (cltpt/combinator::literal "nested") :id nested-keyword)
       (:pattern (cltpt/combinator::word-matcher) :id generic-word)))))

(test org-list-test-2
  (is
   (compare-full-match-loosely
    (org-list-test-2-func)
    '((:ID CLTPT/ORG-MODE:ORG-LIST :BEGIN 0 :END 151 :INDENT 0)))))

(defun org-list-test-3-func ()
  (let* ((text "- we have \\(x=y\\)
   a. nested item one
      more nested text
      1. test1
      2. test2
   b. nested item two
- item three")
         (reader (cltpt/reader:reader-from-string text))
         (parsed-list (cltpt/org-mode::org-list-matcher nil reader 0))
         (html-output (cltpt/org-mode::to-html-list reader parsed-list)))
    html-output))

(test org-list-test-3
  (is
   (equalp
    (org-list-test-3-func)
    "<ul>
<li>we have \\(x=y\\)
<ol type=\"a\">
<li>nested item one
      more nested text
<ol type=\"1\">
<li>test1</li>
<li>test2</li>
</ol>
</li>
<li>nested item two</li>
</ol>
</li>
<li>item three</li>
</ul>
"
    )))

(defun org-list-test-4-func ()
  (let* ((text "- we have \\(x=y\\)
   a. nested item one
      more nested text
      1. test1
      2. test2
   b. nested item two
- item three")
         (reader (cltpt/reader:reader-from-string text))
         (parsed-list (cltpt/org-mode::org-list-matcher nil reader 0))
         (latex-output (cltpt/org-mode::to-latex-list reader parsed-list)))
    latex-output))

(test org-list-test-4
  (let ((result (org-list-test-4-func)))
    (fiveam:is (search "\\begin{itemize}" result))
    (fiveam:is (search "\\end{itemize}" result))))

(defun org-list-test-6-func ()
  (let ((text "- actual cost is 1. the potential changes from \\(2n-M\\) to \\(2(n+1)-M\\) is
  \\[ \\text{amort}(\\text{Insert-Last}) = 1+(2(n+1)-M)-(2n-M) = 1+2 = 3 \\]
- /case 2: array is full (\\(n=M\\))/.
  actual cost is \\(M+1\\) (copy \\(M\\) elements, insert 1). the state changes from \\(\\Phi(M,M)\\) to \\(\\Phi(2M,M+1)\\).
  \\begin{gather*}
    \\Phi_{\\text{before}} = 2M-M = M\\\\
    \\Phi_{\\text{after}} = 2(M+1)-2M = 2\\\\
    \\text{amort}(\\text{Insert-Last}) = (M+1)+(2-M) = 3
  \\end{gather*}"))
    (cltpt/org-mode::org-list-matcher
     nil
     (cltpt/reader:reader-from-string text)
     0
     (cltpt/org-mode::org-mode-inline-text-object-rule))))

(test org-list-test-6
  (let ((result (org-list-test-6-func)))
    (fiveam:is
     (compare-full-match-loosely
      result
      '((:ID CLTPT/ORG-MODE:ORG-LIST :BEGIN 0 :END 469 :INDENT 0))))))

(defun run-org-list-tests ()
  (format t "~&running org-list tests...~%")
  (let ((results (run! 'org-list-suite)))
    (unless results
      (explain! results))))