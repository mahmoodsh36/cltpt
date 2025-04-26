(in-package :cltpt)

;; export org file test
(defun test1 ()
  (time
   (export-org-file
    ;; "/home/mahmooz/brain/notes/1656672670.org"
    "/home/mahmooz/brain/notes/1684594232.org"
    ;; "/home/mahmooz/brain/notes/1707341577.org"
    ;; "/home/mahmooz/work/cltpt/test.org"
    "/home/mahmooz/work/cltpt/test.out.tex")))

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

(defun test-org-parse ()
  (time
   (parse-org-file "/home/mahmooz/brain/notes/1684594232.org")))

;; parse all org files test
(defun test3 ()
  (time (dolist (org-file (directory #P"~/brain/notes/*.org"))
          (format t "doing ~A~%" org-file)
          (let ((result (parse-org-file org-file)))
            ))))

;; grab titles test
(defun test4 ()
  (time
   (format t "number of titles: ~A~%" (length (grab-titles)))))

;; parse nested list
(defun test-parse-list ()
  (org-list-parse "- item one
   extra text for one
- item two
   a. nested item one
      more nested text
   b. nested item two
   c. hi
- item three"))

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
    `(:text (:pattern "[[(%w):(%w)]]")
      :id 'org-link))))

(defun test12 ()
  (find-with-rules
   "#+mykeyword: myvalue"
   (list
    (list :text '(:pattern "#+(%w): (%w)")
          :text-conditions '(begin-of-line)
          :id 'keyword))))

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

;; matching nested pairs with potentially delimiters pairs that should be ignored
;; also nested patterns?
(defun test-parse-3 ()
  (find-with-rules
   "1#(hel7()8lom0559o'((hey)0889)' hey'
there '\"((hey)0889)\" re)h"
   '((:begin (:string "#(")
      :end (:string ")")
      :id outer
      :children ((:begin (:string "(")
                  :end (:string ")")
                  :id inner
                  :children ((:begin (:string "0")
                              :end (:string "9")
                              :id evenmore)))
                 (:begin (:string "7")
                  :end (:string "8")
                  :id nums)
                 (:begin (:string "'")
                  :end (:string "'")
                  :id single-quotes
                  :disallow t
                  :same-line t)
                 (:begin (:string "\"")
                  :end (:string "\"")
                  :id double-quotes))))))

(defun test24 ()
  (find-with-rules
   ":begin:
:id: hey
:end:"
   '((:begin (:pattern ":(%w):")
      :begin-to-hash t
      :begin-conditions (end-of-line not-drawer-end)
      :end (:pattern (%or (:string ":END:")
                      (:string ":end:")))))))

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
(test test-build-forest
  (let ((tree (build-forest '((0 29 id1)
                              (2 27 id2)
                              (10 20 id3)
                              (13 17 id4)
                              (22 25 id5)))))
    (print-forest tree)
    (is (equal
         '(((0 29 ID1) ((2 27 ID2) ((22 25 ID5)) ((10 20 ID3) ((13 17 ID4))))))
         tree))))