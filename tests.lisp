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

;; parse all org files test
(defun test3 ()
  (dolist (org-file (directory #P"~/brain/notes/*.org"))
    (let ((result (parse-org-file org-file)))
      )))

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