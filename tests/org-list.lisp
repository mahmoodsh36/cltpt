(defpackage :cltpt/tests/org-list
  (:use :cl :it.bese.fiveam)
  (:import-from
   :cltpt/tests
   :match-tree-equal-p
   :is-match-tree
   :is-match-trees)
  (:export :run-org-list-tests))

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
  (is-match-tree
   (org-list-test-1-func)
   '((:BEGIN 0 :END 97 :ID CLTPT/ORG-MODE:ORG-LIST)
     ((:BEGIN 0 :END 84 :ID CLTPT/ORG-MODE::LIST-ITEM)
      ((:BEGIN 0 :END 1 :ID CLTPT/ORG-MODE::LIST-ITEM-BULLET))
      ((:BEGIN 2 :END 84 :ID CLTPT/ORG-MODE::LIST-ITEM-CONTENT)
       ((:BEGIN 8 :END 15 :ID MYPAIR)
        ((:BEGIN 0 :END 2))
        ((:BEGIN 5 :END 7)))
       ((:BEGIN 16 :END 82 :ID CLTPT/ORG-MODE:ORG-LIST)
        ((:BEGIN 0 :END 44 :ID CLTPT/ORG-MODE::LIST-ITEM)
         ((:BEGIN 3 :END 5 :ID CLTPT/ORG-MODE::LIST-ITEM-BULLET))
         ((:BEGIN 6 :END 44 :ID CLTPT/ORG-MODE::LIST-ITEM-CONTENT)))
        ((:BEGIN 45 :END 66 :ID CLTPT/ORG-MODE::LIST-ITEM)
         ((:BEGIN 3 :END 5 :ID CLTPT/ORG-MODE::LIST-ITEM-BULLET))
         ((:BEGIN 6 :END 21 :ID CLTPT/ORG-MODE::LIST-ITEM-CONTENT))))))
     ((:BEGIN 85 :END 97 :ID CLTPT/ORG-MODE::LIST-ITEM)
      ((:BEGIN 0 :END 1 :ID CLTPT/ORG-MODE::LIST-ITEM-BULLET))
      ((:BEGIN 2 :END 12 :ID CLTPT/ORG-MODE::LIST-ITEM-CONTENT))))))

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
  (is-match-tree
   (org-list-test-2-func)
   '((:BEGIN 0 :END 151 :ID CLTPT/ORG-MODE:ORG-LIST)
     ((:BEGIN 0 :END 31 :ID CLTPT/ORG-MODE::LIST-ITEM)
      ((:BEGIN 0 :END 1 :ID CLTPT/ORG-MODE::LIST-ITEM-BULLET))
      ((:BEGIN 2 :END 31 :ID CLTPT/ORG-MODE::LIST-ITEM-CONTENT)
       ((:BEGIN 0 :END 4 :ID ITEM-KEYWORD))
       ((:BEGIN 5 :END 8 :ID GENERIC-WORD)
        ((:BEGIN 0 :END 3)))
       ((:BEGIN 11 :END 16 :ID GENERIC-WORD)
        ((:BEGIN 0 :END 5)))
       ((:BEGIN 17 :END 21 :ID GENERIC-WORD)
        ((:BEGIN 0 :END 4)))
       ((:BEGIN 22 :END 25 :ID GENERIC-WORD)
        ((:BEGIN 0 :END 3)))
       ((:BEGIN 26 :END 29 :ID GENERIC-WORD)
        ((:BEGIN 0 :END 3)))))
     ((:BEGIN 32 :END 138 :ID CLTPT/ORG-MODE::LIST-ITEM)
      ((:BEGIN 0 :END 1 :ID CLTPT/ORG-MODE::LIST-ITEM-BULLET))
      ((:BEGIN 2 :END 106 :ID CLTPT/ORG-MODE::LIST-ITEM-CONTENT)
       ((:BEGIN 0 :END 2 :ID GENERIC-WORD)
        ((:BEGIN 0 :END 2)))
       ((:BEGIN 3 :END 7 :ID GENERIC-WORD)
        ((:BEGIN 0 :END 4)))
       ((:BEGIN 10 :END 11 :ID GENERIC-WORD)
        ((:BEGIN 0 :END 1)))
       ((:BEGIN 12 :END 13 :ID GENERIC-WORD)
        ((:BEGIN 0 :END 1)))
       ((:BEGIN 16 :END 104 :ID CLTPT/ORG-MODE:ORG-LIST)
        ((:BEGIN 0 :END 67 :ID CLTPT/ORG-MODE::LIST-ITEM)
         ((:BEGIN 2 :END 4 :ID CLTPT/ORG-MODE::LIST-ITEM-BULLET))
         ((:BEGIN 5 :END 67 :ID CLTPT/ORG-MODE::LIST-ITEM-CONTENT)
          ((:BEGIN 0 :END 6 :ID NESTED-KEYWORD))
          ((:BEGIN 7 :END 11 :ID ITEM-KEYWORD))
          ((:BEGIN 12 :END 15 :ID GENERIC-WORD)
           ((:BEGIN 0 :END 3)))
          ((:BEGIN 21 :END 25 :ID GENERIC-WORD)
           ((:BEGIN 0 :END 4)))
          ((:BEGIN 26 :END 32 :ID NESTED-KEYWORD))
          ((:BEGIN 33 :END 37 :ID GENERIC-WORD)
           ((:BEGIN 0 :END 4)))
          ((:BEGIN 38 :END 62 :ID CLTPT/ORG-MODE:ORG-LIST)
           ((:BEGIN 0 :END 24 :ID CLTPT/ORG-MODE::LIST-ITEM)
            ((:BEGIN 5 :END 7 :ID CLTPT/ORG-MODE::LIST-ITEM-BULLET))
            ((:BEGIN 8 :END 24 :ID CLTPT/ORG-MODE::LIST-ITEM-CONTENT)
             ((:BEGIN 0 :END 4 :ID GENERIC-WORD)
              ((:BEGIN 0 :END 4)))
             ((:BEGIN 5 :END 9 :ID GENERIC-WORD)
              ((:BEGIN 0 :END 4)))
             ((:BEGIN 10 :END 16 :ID NESTED-KEYWORD)))))))
        ((:BEGIN 68 :END 88 :ID CLTPT/ORG-MODE::LIST-ITEM)
         ((:BEGIN 2 :END 4 :ID CLTPT/ORG-MODE::LIST-ITEM-BULLET))
         ((:BEGIN 5 :END 20 :ID CLTPT/ORG-MODE::LIST-ITEM-CONTENT)
          ((:BEGIN 0 :END 6 :ID NESTED-KEYWORD))
          ((:BEGIN 7 :END 11 :ID ITEM-KEYWORD))
          ((:BEGIN 12 :END 15 :ID GENERIC-WORD)
           ((:BEGIN 0 :END 3))))))))
     ((:BEGIN 139 :END 151 :ID CLTPT/ORG-MODE::LIST-ITEM)
      ((:BEGIN 0 :END 1 :ID CLTPT/ORG-MODE::LIST-ITEM-BULLET))
      ((:BEGIN 2 :END 12 :ID CLTPT/ORG-MODE::LIST-ITEM-CONTENT)
       ((:BEGIN 0 :END 4 :ID ITEM-KEYWORD))
       ((:BEGIN 5 :END 10 :ID GENERIC-WORD)
        ((:BEGIN 0 :END 5))))))))

(defun org-list-test-3-func ()
  (let* ((text "- we have \\(x=y\\)
   a. nested item one
      more nested text
      1. test1
      2. test2
   b. nested item two
- item three")
         (parsed (cltpt/base:parse cltpt/org-mode:*org-mode* text))
         (html-output (cltpt/base:convert-tree parsed cltpt/org-mode:*org-mode* cltpt/html:*html*)))
    html-output))

(test org-list-test-3
  (is
   (equalp
    (org-list-test-3-func)
    "<ul>
<li> we have <img src='cltpt-latex-previews/cache/cltpt-snippet-57445dda3eae04b5c8affc6fa2037263.svg' class='inline-math' />
<ol type=\"a\">
   <li> nested item one
      more nested text
<ol type=\"1\">
      <li> test1</li>
      <li> test2</li></ol>
</li>
   <li> nested item two</li></ol>
</li><li> item three</li></ul>
")))

(defun org-list-test-4-func ()
  (let* ((text "- we have \\(x=y\\)
   a. nested item one
      more nested text
      1. test1
      2. test2
   b. nested item two
- item three")
         (parsed (cltpt/base:parse cltpt/org-mode:*org-mode* text))
         (latex-output (cltpt/base:convert-tree parsed cltpt/org-mode:*org-mode* cltpt/latex:*latex*)))
    latex-output))

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
    (is-match-tree
     result
     '((:BEGIN 0 :END 469 :ID CLTPT/ORG-MODE:ORG-LIST)
       ((:BEGIN 0 :END 147 :ID CLTPT/ORG-MODE::LIST-ITEM)
        ((:BEGIN 0 :END 1 :ID CLTPT/ORG-MODE::LIST-ITEM-BULLET))
        ((:BEGIN 2 :END 147 :ID CLTPT/ORG-MODE::LIST-ITEM-CONTENT)
         ((:BEGIN 45 :END 53 :ID CLTPT/LATEX:INLINE-MATH)
          ((:BEGIN 0 :END 2))
          ((:BEGIN 6 :END 8)))
         ((:BEGIN 57 :END 69 :ID CLTPT/LATEX:INLINE-MATH)
          ((:BEGIN 0 :END 2))
          ((:BEGIN 10 :END 12)))
         ((:BEGIN 75 :END 145 :ID CLTPT/LATEX:DISPLAY-MATH)
          ((:BEGIN 0 :END 2))
          ((:BEGIN 68 :END 70)))))
       ((:BEGIN 148 :END 469 :ID CLTPT/ORG-MODE::LIST-ITEM)
        ((:BEGIN 0 :END 1 :ID CLTPT/ORG-MODE::LIST-ITEM-BULLET))
        ((:BEGIN 2 :END 321 :ID CLTPT/ORG-MODE::LIST-ITEM-CONTENT)
         ((:BEGIN 0 :END 33 :ID CLTPT/ORG-MODE::ORG-ITALIC)
          ((:BEGIN 0 :END 1))
          ((:BEGIN 32 :END 33)))
         ((:BEGIN 52 :END 59 :ID CLTPT/LATEX:INLINE-MATH)
          ((:BEGIN 0 :END 2))
          ((:BEGIN 5 :END 7)))
         ((:BEGIN 66 :END 71 :ID CLTPT/LATEX:INLINE-MATH)
          ((:BEGIN 0 :END 2))
          ((:BEGIN 3 :END 5)))
         ((:BEGIN 116 :END 129 :ID CLTPT/LATEX:INLINE-MATH)
          ((:BEGIN 0 :END 2))
          ((:BEGIN 11 :END 13)))
         ((:BEGIN 133 :END 149 :ID CLTPT/LATEX:INLINE-MATH)
          ((:BEGIN 0 :END 2))
          ((:BEGIN 14 :END 16)))
         ((:BEGIN 153 :END 319 :ID CLTPT/LATEX:LATEX-ENV)
          ((:BEGIN 0 :END 15 :ID CLTPT/LATEX::OPEN-TAG)
           ((:BEGIN 0 :END 7))
           ((:BEGIN 7 :END 14))
           ((:BEGIN 14 :END 15)))
          ((:BEGIN 153 :END 166 :ID CLTPT/LATEX::CLOSE-TAG)
           ((:BEGIN 0 :END 5))
           ((:BEGIN 5 :END 12))
           ((:BEGIN 12 :END 13))))))))))

(test list-nested-roundtrip
  "list-match-to-list and list-to-list-string round-trip correctly."
  (dolist (text '("- alpha
- beta
- gamma"
                  "1. first
2. second
3. third"
                  "- parent one
  a. child a
  b. child b
- parent two"
                  "- level 0
  - level 1
    - level 2"))
    (let* ((match (cltpt/org-mode:org-list-matcher
                   nil
                   (cltpt/reader:reader-from-string text)
                   0))
           (data (cltpt/org-mode:list-match-to-list text match))
           (result (cltpt/org-mode:list-to-list-string data)))
      (is (string= text result))))
  ;; verify data structure
  (let* ((text "- parent
  a. child a
  b. child b
- sibling")
         (match (cltpt/org-mode:org-list-matcher
                 nil
                 (cltpt/reader:reader-from-string text)
                 0))
         (data (cltpt/org-mode:list-match-to-list text match)))
    (is (= 2 (length data)))
    (is (string= "parent" (getf (first data) :content)))
    (is (string= "sibling" (getf (second data) :content)))
    (let ((children (getf (first data) :children)))
      (is (= 2 (length children)))
      (is (string= "a." (getf (first children) :bullet)))
      (is (string= "child a" (getf (first children) :content))))))

(defun run-org-list-tests ()
  (format t "~&running org-list tests...~%")
  (let ((results (run! 'org-list-suite)))
    (unless results
      (explain! results))))