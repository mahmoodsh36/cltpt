(defpackage :cltpt/tests
  (:use :cl :it.bese.fiveam :cltpt/org-mode :cltpt/base :cltpt/combinator)
  (:shadowing-import-from :cltpt/combinator parse)
  (:export #:run-cltpt-tests))

(in-package :cltpt/tests)

(def-suite cltpt-suite
  :description "tests for cltpt.")

(in-suite cltpt-suite)

;; (test build-forest-test-1
;;   (let ((entries '((5 15 child-kept)
;;                    (10 25 child-conflict)
;;                    (0 50 parent)
;;                    (30 40 child-ok))))
;;     (is (equal (cltpt/base::build-forest entries)
;;                '(((0 50 parent) ((30 40 child-ok)) ((5 15 child-kept))))))))

;; (test build-forest-test-2
;;   (let ((entries '((5 15 child-kept)
;;                    (10 35 child-conflict)
;;                    (0 50 parent)
;;                    (30 40 child-ok))))
;;     (is (equal (cltpt/base::build-forest entries)
;;                '(((0 50 parent) ((30 40 child-ok)) ((5 15 child-kept))))))))

(defun parse-org-file (filepath)
  ;; we need to "finalize" the classes to be able to use MOP
  (let* ((result (cltpt/base::parse (uiop:read-file-string filepath)
                                    cltpt/org-mode:*org-mode*)))
    result))

(defun test-org-parse ()
  (cltpt/tree:tree-show
   (parse-org-file "test.org")))

(defun test-org-convert ()
  (let ((cltpt/org-mode:*org-enable-macros* t))
    (cltpt/zoo:init)
    (time
     (progn
       ;; (cltpt/base:convert-file
       ;;  (cltpt/base:text-format-by-name "org-mode")
       ;;  (cltpt/base:text-format-by-name "latex")
       ;;  "test3.org"
       ;;  "test.out.tex")
       (cltpt/base:convert-file
        (cltpt/base:text-format-by-name "org-mode")
        (cltpt/base:text-format-by-name "html")
        ;; "/home/mahmooz/brain/notes/1684594232.org"
        ;; "test.org"
        "test2.org"
        "test.out.html")
       nil))))

(defun test-org-convert-1 ()
  (time
   (progn
     ;; (cltpt/base:convert-file
     ;;  (cltpt/base:text-format-by-name "org-mode")
     ;;  (cltpt/base:text-format-by-name "latex")
     ;;  ;; "/home/mahmooz/brain/notes/1752690904.866355.org"
     ;;  "/tmp/test.org"
     ;;  "/tmp/test.out.tex")
     (cltpt/base:convert-file
      (cltpt/base:text-format-by-name "org-mode")
      (cltpt/base:text-format-by-name "html")
      "/home/mahmooz/brain/notes/1712235129.org"
      ;; "/tmp/test.org"
      "/tmp/test.out.html")
     nil)))

(test org-keyword-parse-test
  (is
   (equalp
    (cltpt/combinator::parse
     "#+mykeyword: myvalue"
     (list
      '(:pattern "#+%w: %w")))
    '(((:ID NIL :BEGIN 0 :END 20 :MATCH "#+mykeyword: myvalue")
       ((:BEGIN 0 :END 2 :MATCH "#+")) ((:BEGIN 2 :END 11 :MATCH "mykeyword"))
       ((:BEGIN 11 :END 13 :MATCH ": ")) ((:BEGIN 13 :END 20 :MATCH "myvalue")))))))

(defun inline-latex-test-func ()
  (let ((other-rules (list '(:pattern "%w world" :id keyword))))
    (cltpt/combinator::parse
     "more1 \\(word hello world\\) more2"
     (list
      `(cltpt/combinator::pair
        (:pattern (cltpt/combinator::literal "\\(")
         :id opening)
        (:pattern (cltpt/combinator::literal "\\)")
         :id ending)
        ,other-rules)))))

(test inline-latex-test
  (is (equal '(((:BEGIN 6 :END 26 :MATCH "\\(word hello world\\)")
                ((:ID CLTPT/TESTS::OPENING :BEGIN 6 :END 8 :MATCH "\\("))
                ((:ID KEYWORD :BEGIN 13 :END 24 :MATCH "hello world")
                 ((:BEGIN 13 :END 18 :MATCH "hello")) ((:BEGIN 18 :END 24 :MATCH " world")))
                ((:ID CLTPT/TESTS::ENDING :BEGIN 24 :END 26 :MATCH "\\)"))))
             (cltpt/tests::inline-latex-test-func))))

(defun org-list-test-1-func ()
  (let ((text "- we have \\(x=y\\)
   a. nested item one
      more nested text
   b. nested item two
- item three"))
    (cltpt/org-mode::org-list-matcher
     nil
     text
     0
     '((:pattern (cltpt/combinator::pair
                  (cltpt/combinator::literal "\\(")
                  (cltpt/combinator::literal "\\)")
                  nil)
        :id mypair)))))

(test org-list-test-1
  (is
   (equalp
    (org-list-test-1-func)
    '((:ID CLTPT/ORG-MODE:ORG-LIST :BEGIN 0 :END 97 :MATCH "- we have \\(x=y\\)
   a. nested item one
      more nested text
   b. nested item two
- item three"
       :INDENT 0)
      ((:ID CLTPT/ORG-MODE::LIST-ITEM :INDENT 0 :BEGIN 0 :END 85 :MATCH
        "- we have \\(x=y\\)
   a. nested item one
      more nested text
   b. nested item two
")
       ((:ID CLTPT/ORG-MODE::LIST-ITEM-BULLET :BEGIN 0 :END 2 :MATCH "-"))
       ((:ID CLTPT/ORG-MODE::LIST-ITEM-CONTENT :BEGIN 2 :END 18 :MATCH
         "we have \\(x=y\\)")
        ((:BEGIN 10 :END 17 :ID CLTPT/TESTS::MYPAIR :MATCH "\\(x=y\\)")
         ((:BEGIN 10 :END 12 :MATCH "\\(")) ((:BEGIN 15 :END 17 :MATCH "\\)")))
        ((:ID CLTPT/ORG-MODE:ORG-LIST :BEGIN 18 :END 85 :MATCH "   a. nested item one
      more nested text
   b. nested item two
"
          :INDENT 3)
         ((:ID CLTPT/ORG-MODE::LIST-ITEM :INDENT 3 :BEGIN 18 :END 63 :MATCH
           "   a. nested item one
      more nested text
")
          ((:ID CLTPT/ORG-MODE::LIST-ITEM-BULLET :BEGIN 21 :END 24 :MATCH "a."))
          ((:ID CLTPT/ORG-MODE::LIST-ITEM-CONTENT :BEGIN 24 :END 63 :MATCH
                "nested item one
more nested text")))
         ((:ID CLTPT/ORG-MODE::LIST-ITEM :INDENT 3 :BEGIN 63 :END 85 :MATCH
           "   b. nested item two
")
          ((:ID CLTPT/ORG-MODE::LIST-ITEM-BULLET :BEGIN 66 :END 69 :MATCH "b."))
          ((:ID CLTPT/ORG-MODE::LIST-ITEM-CONTENT :BEGIN 69 :END 85 :MATCH
                "nested item two"))))))
      ((:ID CLTPT/ORG-MODE::LIST-ITEM :INDENT 0 :BEGIN 85 :END 97 :MATCH
        "- item three")
       ((:ID CLTPT/ORG-MODE::LIST-ITEM-BULLET :BEGIN 85 :END 87 :MATCH "-"))
       ((:ID CLTPT/ORG-MODE::LIST-ITEM-CONTENT :BEGIN 87 :END 97 :MATCH
         "item three")))))))

(defun org-list-test-2-func ()
  (let ((text "- item one
  extra text for one
- we have \\(x=y\\)
  a. nested item one
     more nested text
     i. even more nested
  b. nested item two
- item three"))
    (cltpt/combinator::parse
     text
     (list
      '(cltpt/org-mode::org-list-matcher
        ((:pattern (cltpt/combinator::literal "item") :id item-keyword)
         (:pattern (cltpt/combinator::literal "nested") :id nested-keyword)
         (:pattern (cltpt/combinator::word-matcher) :id generic-word)))))))

(test org-list-test-2
  (is
   (equalp
    (org-list-test-2-func)
    '(((:ID CLTPT/ORG-MODE:ORG-LIST :BEGIN 0 :END 151 :MATCH "- item one
  extra text for one
- we have \\(x=y\\)
  a. nested item one
     more nested text
     i. even more nested
  b. nested item two
- item three"
        :INDENT 0)
       ((:ID CLTPT/ORG-MODE::LIST-ITEM :INDENT 0 :BEGIN 0 :END 32 :MATCH "- item one
  extra text for one
")
        ((:ID CLTPT/ORG-MODE::LIST-ITEM-BULLET :BEGIN 0 :END 2 :MATCH "-"))
        ((:ID CLTPT/ORG-MODE::LIST-ITEM-CONTENT :BEGIN 2 :END 32 :MATCH "item one
extra text for one")
         ((:BEGIN 2 :END 6 :ID CLTPT/TESTS::ITEM-KEYWORD :MATCH "item"))
         ((:BEGIN 7 :END 10 :ID CLTPT/TESTS::GENERIC-WORD :MATCH "one"))
         ((:BEGIN 11 :END 16 :ID CLTPT/TESTS::GENERIC-WORD :MATCH "extra"))
         ((:BEGIN 17 :END 21 :ID CLTPT/TESTS::GENERIC-WORD :MATCH "text"))
         ((:BEGIN 22 :END 25 :ID CLTPT/TESTS::GENERIC-WORD :MATCH "for"))
         ((:BEGIN 26 :END 29 :ID CLTPT/TESTS::GENERIC-WORD :MATCH "one"))))
       ((:ID CLTPT/ORG-MODE::LIST-ITEM :INDENT 0 :BEGIN 32 :END 139 :MATCH
         "- we have \\(x=y\\)
  a. nested item one
     more nested text
     i. even more nested
  b. nested item two
")
        ((:ID CLTPT/ORG-MODE::LIST-ITEM-BULLET :BEGIN 32 :END 34 :MATCH "-"))
        ((:ID CLTPT/ORG-MODE::LIST-ITEM-CONTENT :BEGIN 34 :END 50 :MATCH
          "we have \\(x=y\\)")
         ((:BEGIN 34 :END 36 :ID CLTPT/TESTS::GENERIC-WORD :MATCH "we"))
         ((:BEGIN 37 :END 41 :ID CLTPT/TESTS::GENERIC-WORD :MATCH "have"))
         ((:BEGIN 44 :END 45 :ID CLTPT/TESTS::GENERIC-WORD :MATCH "x"))
         ((:BEGIN 46 :END 47 :ID CLTPT/TESTS::GENERIC-WORD :MATCH "y"))
         ((:ID CLTPT/ORG-MODE:ORG-LIST :BEGIN 50 :END 139 :MATCH
           "  a. nested item one
     more nested text
     i. even more nested
  b. nested item two
"
           :INDENT 2)
          ((:ID CLTPT/ORG-MODE::LIST-ITEM :INDENT 2 :BEGIN 50 :END 118 :MATCH
                "  a. nested item one
     more nested text
     i. even more nested
")
           ((:ID CLTPT/ORG-MODE::LIST-ITEM-BULLET :BEGIN 52 :END 55 :MATCH "a."))
           ((:ID CLTPT/ORG-MODE::LIST-ITEM-CONTENT :BEGIN 55 :END 93 :MATCH
                 "nested item one
more nested text")
            ((:BEGIN 55 :END 61 :ID CLTPT/TESTS::NESTED-KEYWORD :MATCH "nested"))
            ((:BEGIN 62 :END 66 :ID CLTPT/TESTS::ITEM-KEYWORD :MATCH "item"))
            ((:BEGIN 67 :END 70 :ID CLTPT/TESTS::GENERIC-WORD :MATCH "one"))
            ((:BEGIN 71 :END 75 :ID CLTPT/TESTS::GENERIC-WORD :MATCH "more"))
            ((:BEGIN 76 :END 82 :ID CLTPT/TESTS::NESTED-KEYWORD :MATCH "nested"))
            ((:BEGIN 83 :END 87 :ID CLTPT/TESTS::GENERIC-WORD :MATCH "text"))
            ((:ID CLTPT/ORG-MODE:ORG-LIST :BEGIN 93 :END 118 :MATCH
                  "     i. even more nested
"
              :INDENT 5)
             ((:ID CLTPT/ORG-MODE::LIST-ITEM :INDENT 5 :BEGIN 93 :END 118 :MATCH
                   "     i. even more nested
")
              ((:ID CLTPT/ORG-MODE::LIST-ITEM-BULLET :BEGIN 98 :END 101 :MATCH
                    "i."))
              ((:ID CLTPT/ORG-MODE::LIST-ITEM-CONTENT :BEGIN 101 :END 118 :MATCH
                    "even more nested")
               ((:BEGIN 101 :END 105 :ID CLTPT/TESTS::GENERIC-WORD :MATCH "even"))
               ((:BEGIN 106 :END 110 :ID CLTPT/TESTS::GENERIC-WORD :MATCH "more"))
               ((:BEGIN 111 :END 117 :ID CLTPT/TESTS::NESTED-KEYWORD :MATCH
                        "nested")))))))
          ((:ID CLTPT/ORG-MODE::LIST-ITEM :INDENT 2 :BEGIN 118 :END 139 :MATCH
                "  b. nested item two
")
           ((:ID CLTPT/ORG-MODE::LIST-ITEM-BULLET :BEGIN 120 :END 123 :MATCH "b."))
           ((:ID CLTPT/ORG-MODE::LIST-ITEM-CONTENT :BEGIN 123 :END 139 :MATCH
                 "nested item two")
            ((:BEGIN 123 :END 129 :ID CLTPT/TESTS::NESTED-KEYWORD :MATCH "nested"))
            ((:BEGIN 130 :END 134 :ID CLTPT/TESTS::ITEM-KEYWORD :MATCH "item"))
            ((:BEGIN 135 :END 138 :ID CLTPT/TESTS::GENERIC-WORD :MATCH "two")))))))
       ((:ID CLTPT/ORG-MODE::LIST-ITEM :INDENT 0 :BEGIN 139 :END 151 :MATCH
         "- item three")
        ((:ID CLTPT/ORG-MODE::LIST-ITEM-BULLET :BEGIN 139 :END 141 :MATCH "-"))
        ((:ID CLTPT/ORG-MODE::LIST-ITEM-CONTENT :BEGIN 141 :END 151 :MATCH
          "item three")
         ((:BEGIN 141 :END 145 :ID CLTPT/TESTS::ITEM-KEYWORD :MATCH "item"))
         ((:BEGIN 146 :END 151 :ID CLTPT/TESTS::GENERIC-WORD :MATCH "three")))))))))

(defun org-list-test-3-func ()
  (let* ((text "- we have \\(x=y\\)
   a. nested item one
      more nested text
      1. test1
      2. test2
   b. nested item two
- item three")
         (parsed-list (cltpt/org-mode::org-list-matcher text 0))
         (html-output (cltpt/org-mode::to-html-list parsed-list)))
    html-output))

(test org-list-test-3
  (is
   (equalp
    (org-list-test-3-func)
    "<ul>
<li>we have \\(x=y\\)<ol type=\"a\">
<li>nested item one
more nested text<ol type=\"1\">
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
         (parsed-list (cltpt/org-mode::org-list-matcher text 0))
         (latex-output (cltpt/org-mode::to-latex-list parsed-list)))
    latex-output))

(defun org-list-test-5-func ()
  (let* ((text "- we have [[mylink]]
   a. nested item one \\(x=y\\)
      more nested text
      1. test1
      2. test2
   b. nested item two
- item three"))
    (cltpt/base::parse
     text
     (list 'cltpt/org-mode::org-list 'cltpt/org-mode::org-link))))

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
     text
     0
     (cltpt/org-mode::org-mode-inline-text-object-rule))))

(defun test-convert-1 ()
  (let ((parsed
          (cltpt/base:parse
           cltpt/org-mode:*org-mode*
           "#(cltpt/base::make-block :type 'theorem :let '((a \"some text\")))
  my first block
  %a
  %(identity a)
  #(cltpt/base::make-block :type 'subtheorem :let '((b \" that will be included on export\")))
    hello
    %(concatenate 'string a b)
  #(cltpt/base::block-end)
#(cltpt/base::block-end)")))
    (cltpt/tree:tree-show parsed)
    (convert-tree parsed cltpt/org-mode:*org-mode* cltpt/html:*html*)))

(defun test-convert-2 ()
  (let ((parsed
          (cltpt/base:parse
           cltpt/org-mode:*org-mode*
           "#(cltpt/base::make-block :type 'theorem :let '((a \"some text\")))
  my block ~here~
  %a
#(cltpt/base::block-end)")))
    (cltpt/tree:tree-show parsed)
    (convert-tree parsed cltpt/org-mode:*org-mode* cltpt/html:*html*)))

(test test-parse-table
  (let ((table
          "| head1 | head2 | head3 |
+------+-------+-------+
|  foo |  bar  |  baz  |
| 123  | 456   | 789   |
+------+-------+-------+
| end  | row   | test  |"))
    (is (equal
         (org-table-parse table)
         '(("head1" "head2" "head3")
           ("foo" "bar" "baz")
           ("123" "456" "789")
           ("end" "row" "test"))))))

(defun test-parse-any-func ()
  (cltpt/combinator::parse
   "[[hello:hey][wow]]"
   (list
    `(:pattern
      (cltpt/combinator::any
       (cltpt/combinator::consec
        (cltpt/combinator::literal "[[")
        (:pattern (cltpt/combinator::word-digits-hyphen) :id test2)
        (cltpt/combinator::literal ":")
        (:pattern (cltpt/combinator::all-but "[]") :id test3)
        (cltpt/combinator::literal "][")
        (cltpt/combinator::all-but "[]")
        (cltpt/combinator::literal "]]"))
       (cltpt/combinator::consec (cltpt/combinator::literal "[[")
                                 (cltpt/combinator::word-digits-hyphen)
                                 (cltpt/combinator::literal "]]"))
       (cltpt/combinator::consec (cltpt/combinator::literal "[[")
                                 (cltpt/combinator::word-digits-hyphen)
                                 (cltpt/combinator::literal ":")
                                 (cltpt/combinator::all-but "[]")
                                 (cltpt/combinator::literal "]]")))
      :id org-link))))

(test test-parse-any
  (is (equal
       (test-parse-any-func)
       '(((:ID CLTPT/TESTS::ORG-LINK :BEGIN 0 :END 18 :MATCH "[[hello:hey][wow]]")
          ((:BEGIN 0 :END 2 :MATCH "[["))
          ((:ID CLTPT/TESTS::TEST2 :BEGIN 2 :END 7 :MATCH "hello"))
          ((:BEGIN 7 :END 8 :MATCH ":"))
          ((:ID CLTPT/TESTS::TEST3 :BEGIN 8 :END 11 :MATCH "hey"))
          ((:BEGIN 11 :END 13 :MATCH "][")) ((:BEGIN 13 :END 16 :MATCH "wow"))
          ((:BEGIN 16 :END 18 :MATCH "]]")))))))

;; matching nested pairs with potentially delimiters pairs that should be ignored
;; also nested patterns?
;; (defun test-parse-3 ()
;;   (find-with-rules
;;    "1#(hel7()8lom0559o'((hey)0889)' hey'
;; there '\"((hey)0889)\" re)h"
;;    '((:begin (:pattern (literal "#("))
;;       :end (:pattern (literal ")"))
;;       :id outer
;;       :children ((:begin (:pattern (literal "("))
;;                   :end (:pattern (literal ")"))
;;                   :id inner
;;                   :children ((:begin (:pattern (literal "0"))
;;                               :end (:pattern (literal "9"))
;;                               :id evenmore)))
;;                  (:begin (:pattern (literal "7"))
;;                   :end (:pattern (literal "8"))
;;                   :id nums)
;;                  (:begin (:pattern (literal "'"))
;;                   :end (:pattern (literal "'"))
;;                   :id single-quotes
;;                   :disallow t
;;                   :nestable nil
;;                   :same-line t)
;;                  (:begin (:pattern (literal "\""))
;;                   :end (:pattern (literal "\""))
;;                   :id double-quotes
;;                   :nestable nil
;;                   ;; :disallow t
;;                   :same-line t))))))

(defun run-cltpt-tests ()
  "runs all defined fiveam test suites for cltpt."
  (format t "~&running cltpt tests...~%")
  (let ((results (run! 'cltpt-suite)))
    (unless results
      (explain! results))))

(defun test-org-table-1 ()
  (let ((text
          "| head1 | head2 | head3 |
+------+-------+-------+
| foo | \\(mymath\\) | baz  |
| 123 | 456          | 789  |
|     |              | 1     |
| end | row          | test |
some more text"))
    (cltpt/org-mode::org-table-matcher
     nil
     text
     0
     '((:pattern (cltpt/combinator::pair
                  (cltpt/combinator::literal "\\(")
                  (cltpt/combinator::literal "\\)")
                  nil)
        :id mypair)))))

(defun test-org-table-2 ()
  (let* ((misaligned-table-text
          "| name | age|
|------+----|
|alice|  25 |
|  bob |30 |
|      |    |
| charlie| 9 |")
         (parse-tree (cltpt/org-mode::org-table-matcher misaligned-table-text 0)))
    (format t "--- original misaligned table ---~%~a~%" misaligned-table-text)
    (when parse-tree
      (let ((formatted-table-text (cltpt/org-mode::reformat-table parse-tree)))
        (format t "~%--- reformatted table ---~%~a" formatted-table-text)))))

(defun test-org-table-3 ()
  (let ((text
          "| head1 | head2 | head3 |
+------+-------+-------+
| foo | \\(mymath\\) | baz  |
| 123 | 456          | 789  |
|     |              | 1     |
| end | row          | test |
some more text"))
    (cltpt/org-mode::to-html-table
     (cltpt/org-mode::org-table-matcher
      text
      0))))

(test test-parse-table
  (let ((table
          "| head1 | head2 | head3 |
+------+-------+-------+
|  foo |  bar  |  baz  |
| 123  | 456   | 789   |
+------+-------+-------+
| end  | row   | test  |"))
    (is (equal
         (org-table-parse table)
         '(("head1" "head2" "head3")
           ("foo" "bar" "baz")
           ("123" "456" "789")
           ("end" "row" "test"))))))

(defun test-pairs-1-func ()
  (let* ((other-rules (list '(:pattern "#+%w" :id keyword)))
         (rules
           `((cltpt/combinator::pair
              (:pattern (cltpt/combinator::literal "(")
               :id opening)
              (:pattern (cltpt/combinator::literal ")")
               :id ending)
              ,other-rules))))
    (cltpt/combinator::scan-all-rules
     nil
     "(my nested (text) (more (#+nested)))"
     rules)))

(test test-pairs-1
  (let ((parser-result (test-pairs-1-func)))
    (fiveam:is
     (equal parser-result
            '(((:BEGIN 0 :END 36 :MATCH "(my nested (text) (more (#+nested)))")
               ((:ID OPENING :BEGIN 0 :END 1 :MATCH "("))
               ((:ID KEYWORD :BEGIN 25 :END 33 :MATCH "#+nested")
                ((:BEGIN 25 :END 27 :MATCH "#+")) ((:BEGIN 27 :END 33 :MATCH "nested")))
               ((:ID ENDING :BEGIN 35 :END 36 :MATCH ")"))))))))

(defun test-sharp-lisp-1-func ()
  (let* ((rules
           '((:pattern
              (cltpt/combinator::consec
               (cltpt/combinator::literal "#")
               (:pattern (cltpt/combinator::lisp-sexp)
                :id lisp-code))
              :id sharp-lisp-block)))
         (input-string "#(format t \"hello)(\\\" there\")"))
    (cltpt/combinator::parse input-string rules)))

(test test-sharp-lisp-1
  (fiveam:is
   (equal (cltpt/tests::test-sharp-lisp-1-func)
          '(((:ID CLTPT/TESTS::SHARP-LISP-BLOCK :BEGIN 0 :END 29 :MATCH
              "#(format t \"hello)(\\\" there\")")
             ((:BEGIN 0 :END 1 :MATCH "#"))
             ((:ID CLTPT/TESTS::LISP-CODE :BEGIN 1 :END 29 :MATCH
               "(format t \"hello)(\\\" there\")" :LISP-FORM (FORMAT T "hello)(\" there")
               :ID CLTPT/COMBINATOR::LISP-FORM-CONTENT)))))))

(defun test-parse-escape-func ()
  (cltpt/combinator::parse
   "this is \\[[hi1:wow][link]] but this is [[hello:hey][wow]] and this \\\\[[hi2:wow][link]]"
   (list
    `(:pattern
      (cltpt/combinator::unescaped
       (cltpt/combinator::any
        (cltpt/combinator::consec
         (cltpt/combinator::literal "[[")
         (:pattern (cltpt/combinator::word-digits-hyphen) :id test2)
         (cltpt/combinator::literal ":")
         (:pattern (cltpt/combinator::all-but "[]") :id test3)
         (cltpt/combinator::literal "][")
         (cltpt/combinator::all-but "[]")
         (cltpt/combinator::literal "]]"))
        (cltpt/combinator::consec (cltpt/combinator::literal "[[")
                                  (cltpt/combinator::word-digits-hyphen)
                                  (cltpt/combinator::literal "]]"))
        (cltpt/combinator::consec (cltpt/combinator::literal "[[")
                                  (cltpt/combinator::word-digits-hyphen)
                                  (cltpt/combinator::literal ":")
                                  (cltpt/combinator::all-but "[]")
                                  (cltpt/combinator::literal "]]"))))
      :id org-link))))

(defun test-pairs-2-func ()
  (let* ((other-rules (list '(:pattern "#+%w" :id keyword)))
         (rules
           `((cltpt/combinator::pair
              (:pattern (cltpt/combinator::unescaped (cltpt/combinator::literal "*"))
               :id openingg)
              (:pattern (cltpt/combinator::literal "*")
               :id endingg)
              ,other-rules
              nil
              nil))))
    (cltpt/combinator::scan-all-rules
     nil
     "\\**my text #+here* *hello there* *more
here* here"
     rules)))

(defun test-pairs-3-func ()
  (let* ((other-rules (list '(:pattern "#+%w" :id keyword)))
         (rules
           `((cltpt/combinator::pair
              (:pattern (cltpt/combinator::unescaped (cltpt/combinator::literal "*"))
               :id openingg)
              (:pattern (cltpt/combinator::literal "*")
               :id endingg)
              ,other-rules
              nil
              nil))))
    (cltpt/combinator::scan-all-rules
     nil
     "\\**my text #+here* *hello there* *more
here* here"
     rules)))

;; end of line
(defun test-eol ()
  (let* ((rule1 (list 'cltpt/combinator::followed-by
                      '(:pattern "#%W" :id hashtag)
                      'cltpt/combinator::at-line-end-p))
         (rules (list rule1)))
    (cltpt/combinator::scan-all-rules
     nil
     "#tag1
a #tag2 in the middle
and a final #tag3"
     rules)))

;; beginning of line
(defun test-bol ()
  (let* ((rule1 (list 'cltpt/combinator::when-match
                      '(:pattern "#%W" :id hashtag)
                      #'cltpt/combinator::at-line-start-p))
         (rules (list rule1)))
    (cltpt/combinator::scan-all-rules
     nil
     "#tag1 is on line 1
this is not a match: #tag2
#tag3 is on line 3"
     rules)))

(defun transformer-test-1-func ()
  (let* ((parsed
           '((:id org-link :begin 89 :end 122 :match "[[mylink1-2:here1][testmore1- 2]]")
             ((:begin 89 :end 91 :match "[["))
             ((:id link-type :begin 91 :end 100 :match "mylink1-2"))
             ((:begin 100 :end 101 :match ":"))
             ((:id link-dest :begin 101 :end 106 :match "here1"))
             ((:begin 106 :end 108 :match "]["))
             ((:id link-desc :begin 108 :end 120 :match "testmore1- 2"))
             ((:begin 120 :end 122 :match "]]"))))
         (dest-rule
           '(cltpt/combinator:consec
             "\\ref{"
             (:pattern (cltpt/combinator:symbol-matcher) :id link-dest)
             "}")))
    (cltpt/transformer:reconstruct-string-from-rule dest-rule parsed)))

(defun transformer-test-2-func ()
  (let* ((parsed
           '((:id org-link :begin 155 :end 177 :match "[[attachment:sliding]]")
             ((:begin 155 :end 157 :match "[["))
             ((:id link-dest :begin 157 :end 175 :match "attachment:sliding"))
             ((:begin 175 :end 177 :match "]]"))))
         (dest-rule
           '(:pattern (cltpt/combinator:consec
                       "\\ref{"
                       (:pattern (cltpt/combinator:symbol-matcher)
                        :id link-dest)
                       "}")
             :id latex-link)))
    (cltpt/transformer:reconstruct-string-from-rule dest-rule parsed)))

(test transformer-test-1
  (fiveam:is
   (string= (cltpt/tests::transformer-test-1-func)
            "\\ref{here1}")))

(defun roam-test-1 ()
  (time
   (let* ((rmr (cltpt/roam:from-files
                '((:path ("/home/mahmooz/brain/notes/" "/home/mahmooz/brain/daily/")
                   :regex ".*\\.org"
                   :format "org-mode")))))
     ;; '((:path ("/home/mahmooz/brain/notes/")
     ;;    :regex "16564.*\\.org"
     ;;    :format "org-mode")))))
     (format t
             "found ~A nodes in a total of ~A documents"
             (length (cltpt/roam:roamer-nodes rmr))
             (length
              (remove-if
               (lambda (node)
                 (cltpt/base:text-object-parent (cltpt/roam:node-text-obj node)))
               (cltpt/roam:roamer-nodes rmr)))))))

(defun agenda-test-1 ()
  (let* ((rmr (cltpt/roam:from-files
                '((:path ("/home/mahmooz/brain/daily/")
                   :regex ".*\\.org"
                   :format "org-mode"))))
         (agenda (cltpt/agenda:from-roamer rmr)))
    agenda))

(defun test-org-timestamp-1 ()
  (cltpt/combinator:match-rule
   nil
   cltpt/org-mode::*org-timestamp-rule*
   "<2023-12-28 Thu>"
   0))

(defun test-org-timestamp-2 ()
  (cltpt/combinator:match-rule
   nil
   cltpt/org-mode::*org-timestamp-rule*
   "<2023-12-28 Thu 18:30:00>"
   0))

(defun test-org-timestamp-3 ()
  (cltpt/combinator:match-rule
   nil
   cltpt/org-mode::*org-timestamp-rule*
   "<2023-12-28 Thu 18:30:00 +1w>"
   0))

(defun test-org-timestamp-4 ()
  (cltpt/combinator:match-rule
   nil
   cltpt/org-mode::*org-timestamp-rule*
   "<2023-12-28 Thu 18:30:00 +1w>--<2023-12-28 Thu 19:00:00 +1w>"
   0))

(defun test-combinator-number-1 ()
  (cltpt/combinator:match-rule
   '(cltpt/combinator:natural-number-matcher)
   "2023"
   0))

(defun org-header-parse-test-1 ()
  (cltpt/combinator::parse
   "
* TODO my main header :test:here:noexport:
SCHEDULED: <2024-10-29 Tue 16:41:04>
CLOSED: [2024-10-29 Tue 16:41:03]
<2025-07-25 Fri 10:00:00>
:PROPERTIES:
:ID: my-id
:LAST_REPEAT: [2024-10-29 Tue 16:40:36]
:END:
"
   (list
    cltpt/org-mode::*org-header-rule*)))

(defun latex-env-parse-test-1 ()
  (cltpt/combinator::parse
   "
\\\\begin{gather}
some math here
\\end{gather}
"
   (list
    cltpt/latex::*latex-env-rule*)))

(defun org-babel-results-parse-test-1 ()
  (cltpt/combinator::parse
   "
#+RESULTS[dbde93ab692f9e8701baf65653d4f407e1852306]:
: 
: \"doing 100k epochs\" 
: loss: 0.07140527340555194
: this should equal 0: #(0.0193214891516944)
: this should equal 1: #(0.9785556472163439)
: this should equal 1: #(0.9836150950875087)
: this should equal 0: #(0.28466690862319854)
: this should equal 0: #(0.014254526557710118)
"
   (list
    cltpt/org-mode::*org-babel-results-rule*)))

(defun roam-convert-test-1 ()
  (time
   (let* ((rmr (cltpt/roam:from-files
                '((:path ("/home/mahmooz/brain/notes/" "/home/mahmooz/brain/daily/")
                   :regex ".*\\.org"
                   :format "org-mode")))))
     ;; '((:path ("/home/mahmooz/brain/notes/")
     ;;    :regex "16564.*\\.org"
     ;;    :format "org-mode")))))
     (cltpt/roam:convert-all rmr (cltpt/base:text-format-by-name "html") "/tmp/out-%(identity title).html"))))

(defun roam-convert-test-2 ()
  (time
   (let* ((rmr (cltpt/roam:from-files
                '((:path ("/home/mahmooz/brain/notes/1710536040.org")
                   :regex ".*\\.org"
                   :format "org-mode")))))
     (cltpt/roam:convert-all rmr (cltpt/base:text-format-by-name "html") "/tmp/out-%(identity title).html"))))

(defun org-block-test-1 ()
  (let* ((text "\\begin{gather}
hey
\\end{gather}
\\\\[ math here \\]
#+begin_src python
  heyi
#+end_src

* DONE does this work

something more
"))
    (cltpt/base::parse
     text
     (cltpt/org-mode:org-mode-text-object-types))))

(defun keywords-test-1 ()
  (cltpt/combinator::parse
   " :hello-there test :hello2 test2"
   (list
    cltpt/org-mode::*keywords-rule*)))

(defun org-src-block-test-1 ()
  (cltpt/combinator::parse
   "
#+begin_src python :results output
  import requests
  print('whatever')
  print('whatever2')
#+end_src

#+RESULTS:
: whatever
: whatever2
: \(11\)
: wow
"
   (list
    cltpt/org-mode::*org-src-block-rule*)))

(defun org-src-block-test-2 ()
  (cltpt/combinator::parse
   "
#+begin_src python :results output
  do nothing
#+end_src

#+RESULTS[ca08ab2a6a58662675694033105ab0b331611fa2]:
[[file:~/brain/out/jyBtMrE.svg]]
"
   (list
    cltpt/org-mode::*org-src-block-rule*)))

(defun test-separated-atleast-one ()
  (cltpt/combinator::parse
   "hi :hello:here:"
   (list
    '(cltpt/combinator:consec
      (cltpt/combinator:atleast-one-discard (cltpt/combinator:literal " "))
      (cltpt/combinator:literal ":")
      (cltpt/combinator:separated-atleast-one
       ":"
       (:pattern
        (cltpt/combinator:symbol-matcher)
        :id tag))
      (cltpt/combinator:literal ":")))))

(defun test-org-latex-env ()
  (cltpt/combinator:parse
   "
#+name: test-name
\\begin{equation}
my equation here
\\end{equation}
"
   (list cltpt/org-mode::*org-latex-env-rule*)))

(defun test-org-latex-env-1 ()
  (cltpt/base:parse
   "
#+name: test-name
\\begin{equation}
my equation here
\\end{equation}
"
   (cltpt/org-mode:org-mode-text-object-types)))

(defun test-bind-and-eval-1 ()
  (cltpt/base:bind-and-eval
   `((title "mytitle"))
   (lambda ()
     (format t "the title is: ~A~%" title))
   :cltpt/tests))

;; this doesnt and shouldnt work
(defun test-bind-and-eval-2 ()
  (cltpt/base:bind-and-eval
   `((title "mytitle"))
   (lambda ()
     (format t "the title is: ~A~%" title))
   :cl-user))

(defun test-org-keyword ()
  (cltpt/combinator:parse
   "
#+title: add vid to github readme
#+date: <2024-04-04 Thu 15:52:09>
#+filetags: 
#+identifier: 1712235129
"
   (list cltpt/org-mode::*org-keyword-rule*)))

;; currently :dvipng seems to be broken
(defun test-latex-preview-1 ()
  (loop for my-comp in (list :latex :lualatex)
        append (loop for my-img-conv in (list :dvisvgm :dvipng :imagemagick)
                     append (let ((cltpt/latex::*latex-compiler-key* my-comp)
                                  (cltpt/latex::*default-latex-preview-pipeline* my-img-conv))
                              (cltpt/latex::generate-previews-for-latex
                               (list "\\(x=\\somebrokencommand\\)"
                                     "\\(x=yyy\\)"))))))

(defun test-latex-preview-2 ()
  (let ((cltpt/latex::*latex-compiler-key* :latex))
    (cltpt/latex::generate-previews-for-latex
     (list "\\(x=\\somebrokencommand123\\)"))))

(defun test-incremental-parsing-1 ()
  (let* ((text "- we have [[mylink]]
   a. nested item one \\(x=y\\)
      more nested text
      1. test1
      2. test2
   b. nested item two
- item three")
         (obj (cltpt/base:parse
               text
               (list 'cltpt/org-mode::org-list 'cltpt/org-mode::org-link)))
         (list-obj (car (cltpt/base:text-object-children obj)))
         (old-list-obj-text (cltpt/base:text-object-text list-obj)))
    (format t "positions before: ~A~%"
            (loop for child in (cltpt/base:text-object-children list-obj)
                  collect (cltpt/base:text-object-begin-in-root child)))
    ;; this simply inserts a word at position 2
    (cltpt/base:handle-changed-regions
     list-obj
     (list 'cltpt/org-mode::org-list 'cltpt/org-mode::org-link)
     (list (cons "hello "
                 (make-region :begin 2 :end 2)))
     t)
    (format t "positions after: ~A~%"
            (loop for child in (cltpt/base:text-object-children list-obj)
                  collect (cltpt/base:text-object-begin-in-root child)))
    (format t
            "   === old ===~%~A~%   === new ===~%~A~%"
            old-list-obj-text
            (cltpt/base:text-object-text list-obj))))

(defun test-incremental-parsing-2 ()
  (let* ((text "some text \\(math here\\) here")
         (doc (cltpt/base:parse
               text
               (list 'cltpt/latex::inline-math)))
         (obj (car (cltpt/base:text-object-children doc)))
         (obj-text (cltpt/base:text-object-text obj)))
    (format t "num of children before: ~A~%" (length (cltpt/base:text-object-children doc)))
    ;; (cltpt/base::handle-change obj (list 'cltpt/latex::inline-math) 2 "\\(new math here\\)")
    (cltpt/base::handle-change obj (list 'cltpt/latex::inline-math) 0 "\\(some\\) \\(math here\\)")
    (format t
            "   === old ===~%~A~%   === new ===~%~A~%"
            obj-text
            (cltpt/base:text-object-text obj))
    (format t "num of children after: ~A~%" (length (cltpt/base:text-object-children doc)))
    (format t "updated doc: ~A~%" (cltpt/base:text-object-text doc))))

(defun test-incremental-parsing-3 ()
  (let* ((text "start \\(math here\\) here")
         (doc (cltpt/base:parse
               (make-text-format "dummy")
               text
               :text-object-types (list 'cltpt/latex::inline-math)))
         (obj (car (cltpt/base:text-object-children doc)))
         (obj-text (cltpt/base:text-object-text obj)))
    (format t "num of children before: ~A~%" (length (cltpt/base:text-object-children doc)))
    (cltpt/base:handle-changed-regions
     doc
     cltpt/org-mode:*org-mode*
     (list
      (cons
       "some"
       (cltpt/base:region-incf
        (cltpt/base:make-region :begin 2 :end 6)
        (length "start ")))
      (cons
       "math"
       (cltpt/base:region-incf
        (cltpt/base:make-region :begin 7 :end 11)
        (length "start ")))
      (cons
       "\\(even more math\\) "
       (cltpt/base:region-incf
        (cltpt/base:make-region :begin 14 :end 14)
        (length "start "))))
     t)
    (format t "num of children after: ~A~%" (length (cltpt/base:text-object-children doc)))
    (format t
            "   === old ===~%~A~%   === new ===~%~A~%"
            obj-text
            (cltpt/base:text-object-text obj))
    (format t "updated doc: ~A~%" (cltpt/base:text-object-text doc))))

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
          (cltpt/outline:render-outline *test-forest*))
  (format t "~%--- 2. ascii style ---~%~a"
          (cltpt/outline:render-outline *test-forest* cltpt/outline:*ascii-style*))
  (format t "~%--- 3. simple style ---~%~a"
          (cltpt/outline:render-outline *test-forest* cltpt/outline:*simple-style*)))

(defun test-outline-2 ()
  (format t "~%--- 1. s-expression output ---~%~a~%"
          (cltpt/outline:render-as-s-expression *test-forest*))
  (format t "~%--- 2. json output ---~%~a~%"
          (cltpt/outline:render-as-json *test-forest*))
  (format t "~%--- 3. graphviz output ---~%")
  (format t "save this as 'tree.dot' and run: dot -Tpng tree.dot -o tree.png~%~a"
          (cltpt/outline:render-as-dot *test-forest*))
  (format t "~%--- 4. path list output ---~%~a"
          (cltpt/outline:render-as-path-list *test-forest*)))

(defun agenda-test-2 ()
  (let* ((rmr (cltpt/roam:from-files
               '((:path ("/home/mahmooz/brain/notes/" "/home/mahmooz/brain/daily/")
                  :regex ".*\\.org"
                  :format "org-mode"))))
         (agenda (cltpt/agenda:from-roamer rmr)))
    (cltpt/agenda::render-agenda agenda)))

(defun test-duration-1 ()
  (let ((duration '(:hour 1 :minute 30)))
    (format t "1. ~A~%" (cltpt/base:add-duration (local-time:now) duration))
    (format t "2. ~A~%" (cltpt/base:add-duration (local-time:now) duration :sign -1))))

(defun test-duration-2 ()
  (cltpt/base:list-date-pairs
   (local-time:today)
   (cltpt/base:add-duration (local-time:today) '(:day 14))
   '(:minute 70)))

(defun agenda-test-3 ()
  (let* ((rmr (cltpt/roam:from-files
               '((:path ("../test.org")
                  :regex ".*\\.org"
                  :format "org-mode"))))
         (agenda (cltpt/agenda:from-roamer rmr)))
    (cltpt/agenda:render-agenda agenda)))