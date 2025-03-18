(in-package :cltpt)

;; export org file test
(defun test1 ()
  (time
   (export-org-file
    ;; "/home/mahmooz/brain/notes/1656672670.org"
    "/home/mahmooz/brain/notes/1684594232.org"
    ;; "/home/mahmooz/brain/notes/1707341577.org"
    ;; "/home/mahmooz/work/cltpt/test.org"
    "/home/mahmooz/work/cltpt/test.out")))

(defun test10 ()
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

;; parse table
(defun test5 ()
  (let ((table
          "| head1 | head2 | head3 |
+------+-------+-------+
|  foo |  bar  |  baz  |
| 123  | 456   | 789   |
+------+-------+-------+
| end  | row   | test  |"))
    (org-table-parse table)))

;; parse nested list
(defun test6 ()
  (org-list-parse "- item one
   extra text for one
- item two
   a. nested item one
      more nested text
   b. nested item two
   c. hi
- item three"))

;; test building a tree from enclsoing indicies
(defun test7 ()
  (let ((tree (build-tree '((0 29 id1)
                            (2 27 id2)
                            (10 20 id3)
                            (13 17 id4)
                            (22 25 id5)))))
    (print-forest tree)))

(defun test8 ()
  (find-multiple-pairs
   "
#+begin_comment
#+begin_test
# #+include: /home/mahmooz/brain/notes/20230520175032-convolutional_neural_network.org
# #+include: /home/mahmooz/brain/notes/20230224163920-common_lisp.org
# #+include: /home/mahmooz/brain/notes/20230503204107-common_lisp_math.org
#+end_test
#+begin_test
#+end_test
#+end_comment
#+include: test
:properties:
:id: hello
:end:
"
   (list
    `(:begin (:regex "#\\+begin_[a-z]+")
      :end   (:regex "#\\+end_[a-z]+")
      :predicate ,(lambda (b e)
                    (string= (subseq b 8) (subseq e 6)))
      :id 'org-block))))

;; this fails, its an edge case
(defun test9 ()
  (find-multiple-pairs
   "#+begin_comment
#+begin_comment
Some text
#+end_hello
#+end_comment
#+end_comment"
   (list
    `(:begin (:regex "#\\+begin_[a-z]+")
      :end   (:regex "#\\+end_[a-z]+")
      :predicate ,(lambda (b e)
                    (string= (subseq b 8) (subseq e 6)))))))

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
      :predicate ,(lambda (b e)
                    (string= (subseq b 8) (subseq e 6)))
      :id 'org-block)
    `(:begin (:pattern "#+begin_(%w)")
      :end   (:pattern "#+end_(%w)")
      :begin-conditions (begin-of-line)
      :end-conditions (begin-of-line)
      :predicate ,(lambda (b e)
                    (string= (subseq b 8) (subseq e 6)))
      :id 'org-blockk)
    (list :text '(:pattern "#+(%w): (%w)")
          :conditions '(begin-of-line)
          :id 'keyword)
    `(:text (:pattern "[[(%w):(%w)]]")
      :id 'org-link))))

(defun test12 ()
  (find-with-rules
   "#+mykeyword: myvalue"
   (list
    (list :text '(:pattern "#+(%w): (%w)")
          :conditions '(begin-of-line)
          :id 'keyword))))

;; extensive example for testing combined rules
(defun test20 ()
  (let ((test-str (concatenate 'string
                               "hello, this is a test." (string #\newline)
                               "  - item 1" (string #\newline)
                               "  - item 2 [[my-link-here:more123][moretext1]]" (string #\newline)
                               "test [[blk:1683060983][multilayer perceptrons]]" (string #\newline)
                               "not a dash." (string #\newline)
                               "BEGIN CODE" (string #\newline)
                               "print('hello')" (string #\newline)
                               "print('world')" (string #\newline)
                               "END CODE" (string #\newline)
                               "more text." (string #\newline)
                               "  - item A" (string #\newline)
                               "  - item B" (string #\newline)
                               "  - item C" (string #\newline)
                               "ERROR: this is an error message." (string #\newline)
                               "some footer text." (string #\newline))))
    (format t "test string:~%~a~%" test-str)
    (let ((result (find-with-rules test-str
                                   (list
                                    ;; region rule: contiguous lines that, after skipping spaces, start with a dash.
                                    '(:region (:string "-")
                                      :ignore " "    ;; ignore spaces at beginning of each line
                                      :id 'dash-region)
                                    ;; begin/end rule: a code block.
                                    '(:begin (:string "BEGIN CODE")
                                      :end   (:string "END CODE")
                                      :id 'code-block)
                                    ;; text rule: lines containing "ERROR:".
                                    '(:text (:string "ERROR:")
                                      :id 'error-text)
                                    '(:text (:pattern "[[(%W-):(%W-)][(%C:abcdefghijklmnopqrstuvwxyz )]]")
                                      :id 'link)))))
          (format t "result: ~a~%" result)
          result)))