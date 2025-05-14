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

(defun test-org-convert ()
  (time
   (progn
     (convert-file
      (text-format-by-name "org-mode")
      (text-format-by-name "latex")
      "test.org"
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

(defun test12 ()
  (find-with-rules
   "#+mykeyword: myvalue"
   (list
    (list :text '(:pattern "#+(%w): (%w)")
          :text-conditions '(begin-of-line)
          :id 'keyword))))

;; this is an edge case, it fails
(defun test8 ()
  (find-with-rules
   "#+begin_comment
#+begin_comment
Some text
#+end_hello
#+end_comment
#+end_comment"
   (list
    `(:begin (:pattern "#+begin_(%w)")
      :end   (:pattern "#+end_(%w)")
      :pair-predicate ,(lambda (b e)
                    (format t "here ~A~%" b)
                    (string= (subseq b 8) (subseq e 6)))
      :id 'org-block))))

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
   (parse
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

;; (test test-parse-table
;;   (let ((table
;;           "| head1 | head2 | head3 |
;; +------+-------+-------+
;; |  foo |  bar  |  baz  |
;; | 123  | 456   | 789   |
;; +------+-------+-------+
;; | end  | row   | test  |"))
;;     (is (equal
;;          (org-table-parse table)
;;          '(("head1" "head2" "head3")
;;            ("foo" "bar" "baz")
;;            ("123" "456" "789")
;;            ("end" "row" "test"))))))

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

(defun test-parse-any ()
  (find-with-rules
   "[[hello:hey][wow]]"
   (list
    `(:text
      (:pattern
       (any
        (consec
         (literal "[[")
         (:pattern (word-digits-hyphen) :id test2)
         (literal ":")
         (:pattern (all-but "[]") :id test3)
         (literal "][") (all-but "[]") (literal "]]"))
        (consec (literal "[[") (word-digits-hyphen) (literal "]]"))
        (consec (literal "[[") (word-digits-hyphen) (literal ":")
                (all-but "[]") (literal "]]"))))
      :id org-link))))

(defun test-parse-4 ()
  (find-with-rules
   "[[hello:hey]]hey"
   (list
    `(:text
      (:pattern
       (consec (literal "[[") (symbol-matcher) (literal ":")
               (all-but "[]") (literal "]]") (only "yeh")))
      :id 'org-link))))

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

(defun test24 ()
  (find-with-rules
   ":begin:
:id: hey
:end:"
   '((:begin ":%w:"
      ;; :begin-to-hash t
      :begin-conditions (end-of-line not-drawer-end)
      :id 'hi
      :end (literal-casein ":end:")))))

(defun run-cltpt-tests ()
  "runs all defined fiveam test suites for cltpt."
  (format t "~&running cltpt tests...~%")
  (let ((results (run! 'cltpt-suite)))
    (unless results
      (explain! results))))