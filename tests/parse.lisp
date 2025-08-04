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
                                    (cltpt/org-mode::org-mode-text-object-types)
                                    :doc-type 'cltpt/org-mode::org-document)))
    result))

(defun test-org-parse ()
  (parse-org-file "test.org"))

(defun test-org-convert ()
  (time
   (progn
     (cltpt/base:convert-file
      (cltpt/base:text-format-by-name "org-mode")
      (cltpt/base:text-format-by-name "latex")
      "test.org"
      "test.out.tex")
     (cltpt/base:convert-file
      (cltpt/base:text-format-by-name "org-mode")
      (cltpt/base:text-format-by-name "html")
      ;; "/home/mahmooz/brain/notes/1684594232.org"
      "test.org"
      "test.out.html")
     nil)))

(defun test-org-convert-1 ()
  (time
   (progn
     (cltpt/base:convert-file
      (cltpt/base:text-format-by-name "org-mode")
      (cltpt/base:text-format-by-name "latex")
      "/home/mahmooz/brain/notes/1684594232.org"
      "/tmp/test.out.tex")
     (cltpt/base:convert-file
      (cltpt/base:text-format-by-name "org-mode")
      (cltpt/base:text-format-by-name "html")
      "/home/mahmooz/brain/notes/1684594232.org"
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

;; latex snippet compilation test
(defun test-latex-svg ()
  (generate-svg-for-latex
   "\\(x=\\sqrt{y}\\)"))

(defun test-convert-1 ()
  (convert-tree
   (cltpt/base::parse
    "#(cltpt/base::make-block :type 'theorem :let '((a \"some text\")))
  my first block
  %a
  #(cltpt/base::make-block :type 'subtheorem :let '((b \" that will be included on export\")))
    hello
    %(concatenate 'string a b)
  #(cltpt/base::block-end)
#(cltpt/base::block-end)"
    (cltpt/org-mode::org-mode-text-object-types))
   (cltpt/base:text-format-by-name "latex")
   (cltpt/org-mode::org-mode-text-object-types)))

(defun test-convert-2 ()
  (convert-tree
   (cltpt/base::parse
    "#(cltpt/base::make-block :type 'theorem :let '((a \"some text\")))
  my first block
  %a
#(cltpt/base::block-end)"
    (cltpt/org-mode::org-mode-text-object-types))
   (cltpt/base:text-format-by-name "latex")
   (cltpt/org-mode::org-mode-text-object-types)))

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
    (cltpt/org-mode::to-latex-table
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
   cltpt/org-mode::*org-timestamp-rule*
   "<2023-12-28 Thu 18:30:00>"
   0))

(defun test-combinator-number-1 ()
  (cltpt/combinator:match-rule
   '(cltpt/combinator:natural-number-matcher)
   "2023"
   0))

(defun org-header-parse-test-1 ()
  (cltpt/combinator::parse
   "
* TODO my main header
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
\\begin{gather}
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