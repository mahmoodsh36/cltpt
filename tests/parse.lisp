(defpackage :cltpt/tests
  (:use :cl :it.bese.fiveam :cltpt/org-mode :cltpt/base :cltpt/combinator)
  (:shadowing-import-from :cltpt/combinator parse)
  (:export #:run-cltpt-tests))

(in-package :cltpt/tests)

(def-suite cltpt-suite
  :description "tests for cltpt.")

(in-suite cltpt-suite)

;; (test org-list-get-bounds-indented
;;   (is (equal (org-list-get-bounds "  - item")
;;              (cons 2 8)))
;;   (is (equal (org-list-get-bounds (format nil "  ~C~C  - properly indented list~C    more stuff for it." #\Newline #\Newline #\Newline))
;;              (cons 6 53))))

;; (test org-list-get-bounds-complex
;;   (let ((text "wew
;; - item one
;;    extra text for one
;; - item two
;;    a. nested item one
;;       more nested text
;;       i. even more nested
;;    b. nested item two
;; - item three
;;    another line for three
;; hey"))
;;     (is (equal (org-list-get-bounds text)
;;                (cons 4 (- (length text) 4))))))

(test org-list-get-bounds-no-list
  (is (equal (org-list-get-bounds "this is not a list.")
             nil))
  (is (equal (org-list-get-bounds "")
             nil))
  (is (equal (org-list-get-bounds "  ")
             nil)))

(test org-list-get-bounds-case2
    (let ((text "preamble text.
  - list item 1 starts here
  - list item 2
    - nested
after list."))
      (is (equal (org-list-get-bounds text) (cons 17 71)))))

(test build-forest-test-1
  (let ((entries '((5 15 child-kept)
                   (10 25 child-conflict)
                   (0 50 parent)
                   (30 40 child-ok))))
    (is (equal (cltpt/base::build-forest entries)
               '(((0 50 parent) ((30 40 child-ok)) ((5 15 child-kept))))))))

(test build-forest-test-2
  (let ((entries '((5 15 child-kept)
                   (10 35 child-conflict)
                   (0 50 parent)
                   (30 40 child-ok))))
    (is (equal (cltpt/base::build-forest entries)
               '(((0 50 parent) ((30 40 child-ok)) ((5 15 child-kept))))))))

(defun parse-org-file (filepath)
  ;; we need to "finalize" the classes to be able to use MOP
  (let* ((result (cltpt/base::parse (uiop:read-file-string filepath)
                                    (cltpt/org-mode::org-mode-text-object-types)
                                    :doc-type 'cltpt/org-mode::org-document)))
    result))

(defun test-org-parse ()
  (parse-org-file "test2.org"))

(defun test-org-convert ()
  (time
   (progn
     (cltpt/base:convert-file
      (cltpt/base:text-format-by-name "org-mode")
      (cltpt/base:text-format-by-name "latex")
      "test2.org"
      "test.out.tex")
     ;; (export-org-file
     ;;  "test.org"
     ;;  "test.out.html"
     ;;  'html)
     nil)))

(defun test11 ()
  (find-with-rules
   "
#+begin_comment
#+begin_test
# #+include: /home/mahmooz/brain/notes/20230520175032-convolutional_neural_network.org
# #+include: /home/mahmooz/brain/notes/20230224163920-common_lisp.org
# #+include: /home/mahmooz/brain/notes/20230503204107-common_lisp_math.org
#+end_test
#+begin_test
#+end_test
[[hello:hey]]
#+end_comment
#+include: test
:properties:
:id: hello
:end:
#+include: test
"
   (list
    `(:begin (:string "#+begin_src")
      :end   (:string "#+end_src")
      :pair-predicate ,(lambda (str b-idx e-idx b-end e-end)
                         (let ((begin-str (subseq str b-idx b-end))
                               (end-str (subseq str e-idx e-end)))
                           (string= (subseq begin-str 8)
                                    (subseq end-str 6))))
      :id 'org-block)
    `(:begin (:pattern "#+begin_(%w)")
      :end   (:pattern "#+end_(%w)")
      :begin-conditions (begin-of-line)
      :end-conditions (begin-of-line)
      :pair-predicate ,(lambda (str b-idx e-idx b-end e-end)
                         (let ((begin-str (subseq str b-idx b-end))
                               (end-str (subseq str e-idx e-end)))
                           (string= (subseq begin-str 8)
                                    (subseq end-str 6))))
      :id 'org-blockk)
    (list :text '(:pattern "#+(%w): (%w)")
          :text-conditions '(begin-of-line)
          :id 'keyword)
    `(:text (:pattern (any (:pattern "[[(%W-):(%E:[])][(%E:[])]]")
                                     (:pattern "[[(%W-)]]")
                                     (:pattern "[[(%W-):(%E:[])]]")))
            :id 'org-link))))

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

(defun org-list-parse-nested-test-func ()
  (let ((text "- we have \\(x=y\\)
   a. nested item one
      more nested text
   b. nested item two
- item three"))
    (cltpt/org-mode::org-list-matcher
     text
     0
     '((cltpt/combinator::pair
        (cltpt/combinator::literal "\\(")
        (cltpt/combinator::literal "\\)")
        nil)))))

(test org-list-parse-nested-test
  (let ((text "- we have \\(x=y\\)
   a. nested item one
      more nested text
   b. nested item two
- item three"))
    (is (equalp (org-list-parse-nested-test-func)
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
                    ((:BEGIN 10 :END 17 :MATCH "\\(x=y\\)") ((:BEGIN 10 :END 12 :MATCH "\\("))
                     ((:BEGIN 15 :END 17 :MATCH "\\)")))
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
                     "item three"))))))))

;; extensive example for testing combined rules
(defun test-parse-1 ()
  (let ((test-str "
hello, this is a test.
  - item 1
  - item 2 [[my-link-here:more123][moretext1]]
test [[blk:1683060983][multilayer perceptrons]]
not a dash.
BEGIN CODE
print('hello')
: some
: more
: \\(mymath\\)
: stuff
print('world')
END CODE
more text.
  - item A
  - item B
  - item C
ERROR: this is an error message.
some footer text."))
    (format t "test string:~%~a~%" test-str)
    (let ((result (find-with-rules test-str
                                   (list
                                    ;; region rule: contiguous lines that, after skipping spaces, start with a dash.
                                    '(:region (:pattern "(%C:- )")
                                      ;; :ignore " "    ;; ignore spaces at beginning of each line
                                      :id 'dash-region)
                                    '(:region (:string ": ")
                                      :id 'colon-region)
                                    ;; begin/end rule: a code block.
                                    '(:begin (:string "BEGIN CODE")
                                      :end   (:string "END CODE")
                                      :id 'code-block)
                                    '(:begin (:string "\\(")
                                      :end   (:string "\\)")
                                      :id 'inline-math)
                                    '(:text (:string "ERROR:")
                                      :id 'error-text)
                                    '(:text (:pattern "[[(%W-):(%W-)][(%C:abcdefghijklmnopqrstuvwxyz123 )]]")
                                      :id 'link)))))
      (format t "result: ~a~%" result)
      result)))

;; latex snippet compilation test
(defun test-latex-svg ()
  (generate-svg-for-latex
   "\\(x=\\sqrt{y}\\)"))

(defun test-parse-2 ()
  (find-with-rules
   "
#+begin_comment
#+begin_test
my text
#+end_test
#+begin_test
some text
#+end_test
[[hello:hey]]
#+end_comment
#+include: test
:properties:
:id: hello
:end:
#+include: test
"
   (list
    `(:begin (:pattern "#+begin_(%w)")
      :end   (:pattern "#+end_(%w)")
      :begin-conditions (begin-of-line)
      :end-conditions (begin-of-line)
      :exclude '(org-link)
      :id 'org-block)
    (list :text '(:pattern "#+(%w): (%w)")
          :text-conditions '(begin-of-line)
          :id 'keyword)
    `(:text (:pattern "[[(%w):(%w)]]")
      :id 'org-link))))

(defun test-convert-1 ()
  (convert-tree
   (cltpt/combinator::parse
    "#(make-block :type 'theorem :let '((a \"some text\")))
  my first block
  %a
  #(make-block :type 'subtheorem :let '((b \" that will be included on export\")))
    hello
    %(concatenate 'string a b)
  #(block-end)
#(block-end)"
    (org-mode-text-object-types))
   'latex
   (org-mode-text-object-types)))

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

;; test building a tree from enclsoing indicies
;; (test test-build-forest
;;   (let ((tree (build-forest '((0 29 id1)
;;                               (2 27 id2)
;;                               (10 20 id3)
;;                               (13 17 id4)
;;                               (22 25 id5)))))
;;     (print-forest tree)
;;     (is (equal
;;          '(((0 29 ID1) ((2 27 ID2) ((22 25 ID5)) ((10 20 ID3) ((13 17 ID4))))))
;;          tree))))

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
          (:BEGIN 0 :END 18 :MATCH "[[hello:hey][wow]]")
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

;; matching nested pairs with potentially delimiters pairs that should be ignored
;; also nested patterns?
(defun test-parse-3 ()
  (find-with-rules
   "1#(hel7()8lom0559o'((hey)0889)' hey'
there '\"((hey)0889)\" re)h"
   '((:begin (:pattern (literal "#("))
      :end (:pattern (literal ")"))
      :id outer
      :children ((:begin (:pattern (literal "("))
                  :end (:pattern (literal ")"))
                  :id inner
                  :children ((:begin (:pattern (literal "0"))
                              :end (:pattern (literal "9"))
                              :id evenmore)))
                 (:begin (:pattern (literal "7"))
                  :end (:pattern (literal "8"))
                  :id nums)
                 (:begin (:pattern (literal "'"))
                  :end (:pattern (literal "'"))
                  :id single-quotes
                  :disallow t
                  :nestable nil
                  :same-line t)
                 (:begin (:pattern (literal "\""))
                  :end (:pattern (literal "\""))
                  :id double-quotes
                  :nestable nil
                  ;; :disallow t
                  :same-line t))))))

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