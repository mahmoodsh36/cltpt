(defpackage :cltpt/tests/org-mode
  (:use :cl :it.bese.fiveam))

(in-package :cltpt/tests/org-mode)

(def-suite org-mode-suite
  :description "tests for org-mode.")

(in-suite org-mode-suite)

(defun org-table-parse (table-text)
  "Parse an org-mode table and return a list of rows, each row being a list of cell values."
  (let ((parsed (cltpt/org-mode::org-table-matcher nil table-text 0)))
    (when parsed
      (loop for row-node in (cdr parsed)
            when (eq (getf (car row-node) :ID) 'CLTPT/ORG-MODE::TABLE-ROW)
              collect (loop for cell-node in (cdr row-node)
                            collect (cltpt/combinator:match-text
                                     (car cell-node)))))))

(defun simplify-match (match)
  (let ((new-match nil))
    (when (getf match :begin)
      (setf (getf new-match :begin) (getf match :begin)))
    (when (getf match :end)
      (setf (getf new-match :end) (getf match :end)))
    ;; add :match (substring between :begin and :end)
    (let ((str (getf match :str))
          (begin (getf match :begin))
          (end (getf match :end)))
      (when (and str begin end)
        (setf (getf new-match :match) (subseq str begin end))))
    new-match))

(defun simplify-full-match (match)
  (cltpt/tree:tree-map match #'simplify-match))

(defun compare-match-loosely (match1 match2)
  (let ((match11 (simplify-match match1))
        (match22 (simplify-match match2)))
    (equalp match11 match22)))

(defun compare-full-match-loosely (match1 match2)
  (cltpt/tree::trees-map
   (list match1 match2)
   (lambda (submatch1 submatch2)
     (compare-match-loosely (car submatch1)
                            (car submatch2)))))

(defun org-keyword-basic-func ()
  (let ((result (cltpt/combinator:parse
                 "#+title: My Document"
                 (list cltpt/org-mode::*org-keyword-rule*))))
    (compare-full-match-loosely
     (car result)
     '((:BEGIN 0 :END 20 :ID CLTPT/ORG-MODE::ORG-KEYWORD)
       ((:BEGIN 0 :END 2 :MATCH "#+"))
       ((:BEGIN 2 :END 7 :ID KEYWORD :MATCH "title"))
       ((:BEGIN 7 :END 8 :MATCH ":"))
       ((:BEGIN 8 :END 9 :MATCH " "))
       ((:BEGIN 9 :END 20 :ID VALUE :MATCH "My Document"))))))

(test org-keyword-basic
  (fiveam:is (org-keyword-basic-func)))

(defun org-keyword-with-empty-value-func ()
  (let ((result (cltpt/combinator:parse
                 "#+filetags: "
                 (list cltpt/org-mode::*org-keyword-rule*))))
    (compare-full-match-loosely
     (car result)
     '((:BEGIN 0 :END 12 :ID CLTPT/ORG-MODE::ORG-KEYWORD)
       ((:BEGIN 0 :END 2 :MATCH "#+"))
       ((:BEGIN 2 :END 10 :ID KEYWORD :MATCH "filetags"))
       ((:BEGIN 10 :END 11 :MATCH ":"))
       ((:BEGIN 11 :END 12 :MATCH " "))
       ((:BEGIN 12 :END 12 :ID VALUE :MATCH ""))))))

(test org-keyword-with-empty-value
  (fiveam:is (org-keyword-with-empty-value-func)))

(defun org-keyword-multiword-func ()
  (let ((result (cltpt/combinator:parse
                 "#+author: John Doe and Jane Smith"
                 (list cltpt/org-mode::*org-keyword-rule*))))
    (compare-full-match-loosely
     (car result)
     '((:BEGIN 0 :END 33 :ID CLTPT/ORG-MODE::ORG-KEYWORD)
       ((:BEGIN 0 :END 2 :MATCH "#+"))
       ((:BEGIN 2 :END 8 :ID KEYWORD :MATCH "author"))
       ((:BEGIN 8 :END 9 :MATCH ":"))
       ((:BEGIN 9 :END 10 :MATCH " "))
       ((:BEGIN 10 :END 33 :ID VALUE :MATCH "John Doe and Jane Smith"))))))

(test org-keyword-multiword
  (fiveam:is (org-keyword-multiword-func)))

(defun org-keyword-comprehensive-func ()
  (let ((result (cltpt/combinator:parse
                 "#+mykeyword: myvalue"
                 (list cltpt/org-mode::*org-keyword-rule*))))
    (compare-full-match-loosely
     (car result)
     '((:BEGIN 0 :END 20 :STR "#+mykeyword: myvalue")
       ((:BEGIN 0 :END 12 :STR "#+mykeyword: myvalue")
        ((:BEGIN 0 :END 2 :STR "#+mykeyword: myvalue"))
        ((:BEGIN 2 :END 11 :STR "#+mykeyword: myvalue"))
        ((:BEGIN 11 :END 12 :STR "#+mykeyword: myvalue")))
       ((:BEGIN 12 :END 13 :STR "#+mykeyword: myvalue"))
       ((:BEGIN 13 :END 20 :STR "#+mykeyword: myvalue"))))))

(test org-keyword-comprehensive
  (fiveam:is (org-keyword-comprehensive-func)))

(defun org-comment-basic-func ()
  (let ((result (cltpt/combinator:parse
                 "# this is a comment"
                 (list cltpt/org-mode::*org-comment-rule*))))
    (compare-full-match-loosely
     (car result)
     '((:BEGIN 0 :END 19 :ID CLTPT/ORG-MODE::ORG-COMMENT)
       ((:BEGIN 0 :END 1 :MATCH "#"))
       ((:BEGIN 1 :END 2 :MATCH " "))
       ((:BEGIN 2 :END 19 :MATCH "this is a comment"))))))

(test org-comment-basic
  (fiveam:is (org-comment-basic-func)))

(defun org-comment-not-at-line-start-func ()
  (let ((result (cltpt/combinator:parse
                 "text # this should not match"
                 (list cltpt/org-mode::*org-comment-rule*))))
    (= (length result) 0)))

(test org-comment-not-at-line-start
  (fiveam:is (org-comment-not-at-line-start-func)))

(defun org-header-basic-func ()
  (let ((result (cltpt/combinator:parse
                 "* my header"
                 (list cltpt/org-mode::*org-header-rule*))))
    (compare-full-match-loosely
     (car result)
     '((:BEGIN 0 :END 9 :ID CLTPT/ORG-MODE::ORG-HEADER)
       ((:BEGIN 0 :END 1 :ID STARS :MATCH "*"))
       ((:BEGIN 1 :END 2 :MATCH " "))
       ((:BEGIN 2 :END 9 :ID TITLE :MATCH "my header"))))))

(test org-header-basic
  (fiveam:is (org-header-basic-func)))

(defun org-header-with-todo-func ()
  (let ((result (cltpt/combinator:parse
                 "* TODO my header"
                 (list cltpt/org-mode::*org-header-rule*))))
    (compare-full-match-loosely
     (car result)
     '((:BEGIN 0 :END 14 :ID CLTPT/ORG-MODE::ORG-HEADER)
       ((:BEGIN 0 :END 1 :ID STARS :MATCH "*"))
       ((:BEGIN 1 :END 2 :MATCH " "))
       ((:BEGIN 2 :END 6 :ID TODO :MATCH "TODO"))
       ((:BEGIN 6 :END 7 :MATCH " "))
       ((:BEGIN 7 :END 14 :ID TITLE :MATCH "my header"))))))

(test org-header-with-todo
  (fiveam:is (org-header-with-todo-func)))

(defun org-header-with-tags-func ()
  (let ((result (cltpt/combinator:parse
                 "* my header :tag1:tag2:"
                 (list cltpt/org-mode::*org-header-rule*))))
    (compare-full-match-loosely
     (car result)
     '((:BEGIN 0 :END 21 :ID CLTPT/ORG-MODE::ORG-HEADER)
       ((:BEGIN 0 :END 1 :ID STARS :MATCH "*"))
       ((:BEGIN 1 :END 2 :MATCH " "))
       ((:BEGIN 2 :END 12 :ID TITLE :MATCH "my header "))
       ((:BEGIN 12 :END 21 :ID TAGS :MATCH ":tag1:tag2:"))))))

(test org-header-with-tags
  (fiveam:is (org-header-with-tags-func)))

;; comprehensive header test - more extensive with scheduling/closing
(defun org-header-comprehensive-test-func ()
  (let ((result (cltpt/combinator::parse
                 "* TODO my main header :test:here:noexport:
SCHEDULED: <2024-10-29 Tue 16:41:04>
CLOSED: [2024-10-29 Tue 16:41:03]
<2025-07-25 Fri 10:00:00>
:PROPERTIES:
:ID: my-id
:LAST_REPEAT: [2024-10-29 Tue 16:40:36]
:END:"
                 (list cltpt/org-mode::*org-header-rule*))))
    (compare-full-match-loosely
     (car result)
     '((:BEGIN 0 :END 137 :ID CLTPT/ORG-MODE::ORG-HEADER)
       ((:BEGIN 0 :END 1 :ID STARS :MATCH "*"))
       ((:BEGIN 1 :END 2 :MATCH " "))
       ((:BEGIN 2 :END 6 :ID TODO :MATCH "TODO"))
       ((:BEGIN 6 :END 7 :MATCH " "))
       ((:BEGIN 7 :END 23 :ID TITLE :MATCH "my main header "))
       ((:BEGIN 23 :END 39 :ID TAGS :MATCH ":test:here:noexport:"))
       ;; More children for the schedule, closed, properties, etc.
       ))))

(test org-header-comprehensive
  (fiveam:is (org-header-comprehensive-test-func)))

(defun org-timestamp-date-only-func ()
  (let ((result (cltpt/combinator:parse
                 "<2024-01-15 Mon>"
                 (list cltpt/org-mode::*org-timestamp-rule*))))
    (compare-full-match-loosely
     (car result)
     '((:BEGIN 0 :END 16 :MATCH "<2024-01-15 Mon>")
       ((:BEGIN 0 :END 16 :MATCH "<2024-01-15 Mon>")
        ((:BEGIN 0 :END 1 :MATCH "<"))
        ((:BEGIN 1 :END 5 :MATCH "2024"))
        ((:BEGIN 5 :END 6 :MATCH "-"))
        ((:BEGIN 6 :END 8 :MATCH "01"))
        ((:BEGIN 8 :END 9 :MATCH "-"))
        ((:BEGIN 9 :END 11 :MATCH "15"))
        ((:BEGIN 11 :END 12 :MATCH " "))
        ((:BEGIN 12 :END 15 :MATCH "Mon"))
        ((:BEGIN 15 :END 16 :MATCH ">")))))))

(test org-timestamp-date-only
  (fiveam:is (org-timestamp-date-only-func)))

(defun org-timestamp-with-time-func ()
  (let ((result (cltpt/combinator:parse
                 "<2024-01-15 Mon 14:30:00>"
                 (list cltpt/org-mode::*org-timestamp-rule*))))
    (compare-full-match-loosely
     (car result)
     '((:BEGIN 0 :END 25 :MATCH "<2024-01-15 Mon 14:30:00>")
       ((:BEGIN 0 :END 25 :MATCH "<2024-01-15 Mon 14:30:00>")
        ((:BEGIN 0 :END 1 :MATCH "<"))
        ((:BEGIN 1 :END 5 :MATCH "2024"))
        ((:BEGIN 5 :END 6 :MATCH "-"))
        ((:BEGIN 6 :END 8 :MATCH "01"))
        ((:BEGIN 8 :END 9 :MATCH "-"))
        ((:BEGIN 9 :END 11 :MATCH "15"))
        ((:BEGIN 11 :END 12 :MATCH " "))
        ((:BEGIN 12 :END 15 :MATCH "Mon"))
        ((:BEGIN 15 :END 16 :MATCH " "))
        ((:BEGIN 16 :END 18 :MATCH "14"))
        ((:BEGIN 18 :END 19 :MATCH ":"))
        ((:BEGIN 19 :END 21 :MATCH "30"))
        ((:BEGIN 21 :END 22 :MATCH ":"))
        ((:BEGIN 22 :END 24 :MATCH "00"))
        ((:BEGIN 24 :END 25 :MATCH ">")))))))

(test org-timestamp-with-time
  (fiveam:is (org-timestamp-with-time-func)))

(defun org-timestamp-with-repeater-func ()
  (let ((result (cltpt/combinator:parse
                 "<2024-01-15 Mon 14:30:00 +1w>"
                 (list cltpt/org-mode::*org-timestamp-rule*))))
    (compare-full-match-loosely
     (car result)
     '((:BEGIN 0 :END 29 :MATCH "<2024-01-15 Mon 14:30:00 +1w>")
       ((:BEGIN 0 :END 29 :MATCH "<2024-01-15 Mon 14:30:00 +1w>")
        ((:BEGIN 0 :END 1 :MATCH "<"))
        ((:BEGIN 1 :END 5 :MATCH "2024"))
        ((:BEGIN 5 :END 6 :MATCH "-"))
        ((:BEGIN 6 :END 8 :MATCH "01"))
        ((:BEGIN 8 :END 9 :MATCH "-"))
        ((:BEGIN 9 :END 11 :MATCH "15"))
        ((:BEGIN 11 :END 12 :MATCH " "))
        ((:BEGIN 12 :END 15 :MATCH "Mon"))
        ((:BEGIN 15 :END 16 :MATCH " "))
        ((:BEGIN 16 :END 18 :MATCH "14"))
        ((:BEGIN 18 :END 19 :MATCH ":"))
        ((:BEGIN 19 :END 21 :MATCH "30"))
        ((:BEGIN 21 :END 22 :MATCH ":"))
        ((:BEGIN 22 :END 24 :MATCH "00"))
        ((:BEGIN 24 :END 25 :MATCH " "))
        ((:BEGIN 25 :END 28 :MATCH "+1w"))
        ((:BEGIN 28 :END 29 :MATCH ">")))))))

(test org-timestamp-with-repeater
  (fiveam:is (org-timestamp-with-repeater-func)))

(defun org-timestamp-range-func ()
  (let ((result (cltpt/combinator:parse
                 "<2024-01-15 Mon>--<2024-01-16 Tue>"
                 (list cltpt/org-mode::*org-timestamp-rule*))))
    (compare-full-match-loosely
     (car result)
     '((:BEGIN 0 :END 34 :MATCH "<2024-01-15 Mon>--<2024-01-16 Tue>")
       ((:BEGIN 0 :END 34 :MATCH "<2024-01-15 Mon>--<2024-01-16 Tue>")
        ((:BEGIN 0 :END 1 :MATCH "<"))
        ((:BEGIN 1 :END 5 :MATCH "2024"))
        ((:BEGIN 5 :END 6 :MATCH "-"))
        ((:BEGIN 6 :END 8 :MATCH "01"))
        ((:BEGIN 8 :END 9 :MATCH "-"))
        ((:BEGIN 9 :END 11 :MATCH "15"))
        ((:BEGIN 11 :END 12 :MATCH " "))
        ((:BEGIN 12 :END 15 :MATCH "Mon"))
        ((:BEGIN 15 :END 16 :MATCH ">"))
        ((:BEGIN 16 :END 18 :MATCH "--"))
        ((:BEGIN 18 :END 19 :MATCH "<"))
        ((:BEGIN 19 :END 23 :MATCH "2024"))
        ((:BEGIN 23 :END 24 :MATCH "-"))
        ((:BEGIN 24 :END 26 :MATCH "01"))
        ((:BEGIN 26 :END 27 :MATCH "-"))
        ((:BEGIN 27 :END 29 :MATCH "16"))
        ((:BEGIN 29 :END 30 :MATCH " "))
        ((:BEGIN 30 :END 33 :MATCH "Tue"))
        ((:BEGIN 33 :END 34 :MATCH ">")))))))

(test org-timestamp-range
  (fiveam:is (org-timestamp-range-func)))

(defun org-timestamp-comprehensive-date-only-func ()
  (compare-full-match-loosely
   (cltpt/combinator:match-rule nil cltpt/org-mode::*org-timestamp-rule* "<2023-12-28 Thu>" 0)
   '((:BEGIN 0 :END 16 :MATCH "<2023-12-28 Thu>")
     ((:BEGIN 0 :END 16 :ID CLTPT/ORG-MODE::BEGIN :MATCH "<2023-12-28 Thu>")
      ((:BEGIN 0 :END 1 :MATCH "<"))
      ((:BEGIN 1 :END 5 :ID CLTPT/ORG-MODE::YEAR :MATCH "2023"))
      ((:BEGIN 5 :END 6 :MATCH "-"))
      ((:BEGIN 6 :END 8 :ID CLTPT/ORG-MODE::MONTH :MATCH "12"))
      ((:BEGIN 8 :END 9 :MATCH "-"))
      ((:BEGIN 9 :END 11 :ID CLTPT/ORG-MODE::DAY :MATCH "28"))
      ((:BEGIN 11 :END 12 :MATCH " "))
      ((:BEGIN 12 :END 15 :ID CLTPT/ORG-MODE::WEEKDAY :MATCH "Thu"))
      ((:BEGIN 15 :END 16 :MATCH ">"))))))

(test org-timestamp-comprehensive-date-only
  (fiveam:is (org-timestamp-comprehensive-date-only-func)))

(defun org-timestamp-comprehensive-with-time-func ()
  (compare-full-match-loosely
   (cltpt/combinator:match-rule nil cltpt/org-mode::*org-timestamp-rule* "<2023-12-28 Thu 18:30:00>" 0)
   '((:BEGIN 0 :END 25 :MATCH "<2023-12-28 Thu 18:30:00>")
     ((:BEGIN 0 :END 25 :ID CLTPT/ORG-MODE::BEGIN :MATCH "<2023-12-28 Thu 18:30:00>")
      ((:BEGIN 0 :END 1 :MATCH "<"))
      ((:BEGIN 1 :END 5 :ID CLTPT/ORG-MODE::YEAR :MATCH "2023"))
      ((:BEGIN 5 :END 6 :MATCH "-"))
      ((:BEGIN 6 :END 8 :ID CLTPT/ORG-MODE::MONTH :MATCH "12"))
      ((:BEGIN 8 :END 9 :MATCH "-"))
      ((:BEGIN 9 :END 11 :ID CLTPT/ORG-MODE::DAY :MATCH "28"))
      ((:BEGIN 11 :END 12 :MATCH " "))
      ((:BEGIN 12 :END 15 :ID CLTPT/ORG-MODE::WEEKDAY :MATCH "Thu"))
      ((:BEGIN 15 :END 24 :MATCH " 18:30:00")
       ((:BEGIN 15 :END 16 :MATCH " "))
       ((:BEGIN 16 :END 18 :ID CLTPT/ORG-MODE::HOUR :MATCH "18"))
       ((:BEGIN 18 :END 19 :MATCH ":"))
       ((:BEGIN 19 :END 21 :ID CLTPT/ORG-MODE::MINUTE :MATCH "30"))
       ((:BEGIN 21 :END 22 :MATCH ":"))
       ((:BEGIN 22 :END 24 :ID SECOND :MATCH "00")))
      ((:BEGIN 24 :END 25 :MATCH ">"))))))

(test org-timestamp-comprehensive-with-time
  (fiveam:is (org-timestamp-comprehensive-with-time-func)))

(defun org-timestamp-comprehensive-with-repeater-func ()
  (compare-full-match-loosely
   (cltpt/combinator:match-rule nil cltpt/org-mode::*org-timestamp-rule* "<2023-12-28 Thu 18:30:00 +1w>" 0)
   '((:BEGIN 0 :END 29 :MATCH "<2023-12-28 Thu 18:30:00 +1w>")
     ((:BEGIN 0 :END 29 :ID CLTPT/ORG-MODE::BEGIN :MATCH "<2023-12-28 Thu 18:30:00 +1w>")
      ((:BEGIN 0 :END 1 :MATCH "<"))
      ((:BEGIN 1 :END 5 :ID CLTPT/ORG-MODE::YEAR :MATCH "2023"))
      ((:BEGIN 5 :END 6 :MATCH "-"))
      ((:BEGIN 6 :END 8 :ID CLTPT/ORG-MODE::MONTH :MATCH "12"))
      ((:BEGIN 8 :END 9 :MATCH "-"))
      ((:BEGIN 9 :END 11 :ID CLTPT/ORG-MODE::DAY :MATCH "28"))
      ((:BEGIN 11 :END 12 :MATCH " "))
      ((:BEGIN 12 :END 15 :ID CLTPT/ORG-MODE::WEEKDAY :MATCH "Thu"))
      ((:BEGIN 15 :END 24 :MATCH " 18:30:00")
       ((:BEGIN 15 :END 16 :MATCH " "))
       ((:BEGIN 16 :END 18 :ID CLTPT/ORG-MODE::HOUR :MATCH "18"))
       ((:BEGIN 18 :END 19 :MATCH ":"))
       ((:BEGIN 19 :END 21 :ID CLTPT/ORG-MODE::MINUTE :MATCH "30"))
       ((:BEGIN 21 :END 22 :MATCH ":"))
       ((:BEGIN 22 :END 24 :ID SECOND :MATCH "00")))
      ((:BEGIN 24 :END 28 :MATCH " +1w")
       ((:BEGIN 24 :END 26 :MATCH " +"))
       ((:BEGIN 26 :END 27 :ID CLTPT/ORG-MODE::REPEAT-NUM :MATCH "1"))
       ((:BEGIN 27 :END 28 :ID CLTPT/ORG-MODE::REPEAT-WORD :MATCH "w")))
      ((:BEGIN 28 :END 29 :MATCH ">"))))))

(test org-timestamp-comprehensive-with-repeater
  (fiveam:is (org-timestamp-comprehensive-with-repeater-func)))

(defun org-timestamp-comprehensive-range-func ()
  (compare-full-match-loosely
   (cltpt/combinator:match-rule nil cltpt/org-mode::*org-timestamp-rule* "<2023-12-28 Thu 18:30:00 +1w>--<2023-12-28 Thu 19:00:00 +1w>" 0)
   '((:BEGIN 0 :END 60 :MATCH "<2023-12-28 Thu 18:30:00 +1w>--<2023-12-28 Thu 19:00:00 +1w>")
     ((:BEGIN 0 :END 29 :ID CLTPT/ORG-MODE::BEGIN :MATCH "<2023-12-28 Thu 18:30:00 +1w>")
      ((:BEGIN 0 :END 1 :MATCH "<"))
      ((:BEGIN 1 :END 5 :ID CLTPT/ORG-MODE::YEAR :MATCH "2023"))
      ((:BEGIN 5 :END 6 :MATCH "-"))
      ((:BEGIN 6 :END 8 :ID CLTPT/ORG-MODE::MONTH :MATCH "12"))
      ((:BEGIN 8 :END 9 :MATCH "-"))
      ((:BEGIN 9 :END 11 :ID CLTPT/ORG-MODE::DAY :MATCH "28"))
      ((:BEGIN 11 :END 12 :MATCH " "))
      ((:BEGIN 12 :END 15 :ID CLTPT/ORG-MODE::WEEKDAY :MATCH "Thu"))
      ((:BEGIN 15 :END 24 :MATCH " 18:30:00")
       ((:BEGIN 15 :END 16 :MATCH " "))
       ((:BEGIN 16 :END 18 :ID CLTPT/ORG-MODE::HOUR :MATCH "18"))
       ((:BEGIN 18 :END 19 :MATCH ":"))
       ((:BEGIN 19 :END 21 :ID CLTPT/ORG-MODE::MINUTE :MATCH "30"))
       ((:BEGIN 21 :END 22 :MATCH ":"))
       ((:BEGIN 22 :END 24 :ID SECOND :MATCH "00")))
      ((:BEGIN 24 :END 28 :MATCH " +1w")
       ((:BEGIN 24 :END 26 :MATCH " +"))
       ((:BEGIN 26 :END 27 :ID CLTPT/ORG-MODE::REPEAT-NUM :MATCH "1"))
       ((:BEGIN 27 :END 28 :ID CLTPT/ORG-MODE::REPEAT-WORD :MATCH "w")))
      ((:BEGIN 28 :END 29 :MATCH ">")))
     ((:BEGIN 29 :END 31 :MATCH "--"))
     ((:BEGIN 31 :END 60 :ID CLTPT/ORG-MODE::END :MATCH "<2023-12-28 Thu 19:00:00 +1w>")
      ((:BEGIN 31 :END 32 :MATCH "<"))
      ((:BEGIN 32 :END 36 :ID CLTPT/ORG-MODE::YEAR :MATCH "2023"))
      ((:BEGIN 36 :END 37 :MATCH "-"))
      ((:BEGIN 37 :END 39 :ID CLTPT/ORG-MODE::MONTH :MATCH "12"))
      ((:BEGIN 39 :END 40 :MATCH "-"))
      ((:BEGIN 40 :END 42 :ID CLTPT/ORG-MODE::DAY :MATCH "28"))
      ((:BEGIN 42 :END 43 :MATCH " "))
      ((:BEGIN 43 :END 46 :ID CLTPT/ORG-MODE::WEEKDAY :MATCH "Thu"))
      ((:BEGIN 46 :END 55 :MATCH " 19:00:00")
       ((:BEGIN 46 :END 47 :MATCH " "))
       ((:BEGIN 47 :END 49 :ID CLTPT/ORG-MODE::HOUR :MATCH "19"))
       ((:BEGIN 49 :END 50 :MATCH ":"))
       ((:BEGIN 50 :END 52 :ID CLTPT/ORG-MODE::MINUTE :MATCH "00"))
       ((:BEGIN 52 :END 53 :MATCH ":"))
       ((:BEGIN 53 :END 55 :ID SECOND :MATCH "00")))
      ((:BEGIN 55 :END 59 :MATCH " +1w")
       ((:BEGIN 55 :END 57 :MATCH " +"))
       ((:BEGIN 57 :END 58 :ID CLTPT/ORG-MODE::REPEAT-NUM :MATCH "1"))
       ((:BEGIN 58 :END 59 :ID CLTPT/ORG-MODE::REPEAT-WORD :MATCH "w")))
      ((:BEGIN 59 :END 60 :MATCH ">"))))))

(test org-timestamp-comprehensive-range
  (fiveam:is (org-timestamp-comprehensive-range-func)))

(defun org-single-timestamp-basic-func ()
  (let ((result (cltpt/combinator:parse
                 "<2024-01-15 Mon>"
                 (list cltpt/org-mode::*org-single-timestamp-rule*))))
    (compare-full-match-loosely
     (car result)
     '((:BEGIN 0 :END 16 :MATCH "<2024-01-15 Mon>")
       ((:BEGIN 0 :END 1 :MATCH "<"))
       ((:BEGIN 1 :END 5 :MATCH "2024"))
       ((:BEGIN 5 :END 6 :MATCH "-"))
       ((:BEGIN 6 :END 8 :MATCH "01"))
       ((:BEGIN 8 :END 9 :MATCH "-"))
       ((:BEGIN 9 :END 11 :MATCH "15"))
       ((:BEGIN 11 :END 12 :MATCH " "))
       ((:BEGIN 12 :END 15 :MATCH "Mon"))
       ((:BEGIN 15 :END 16 :MATCH ">"))))))

(test org-single-timestamp-basic
  (fiveam:is (org-single-timestamp-basic-func)))

(defun org-timestamp-bracket-basic-func ()
  (let ((result (cltpt/combinator:parse
                 "[2024-01-15 Mon]"
                 (list cltpt/org-mode::*org-timestamp-bracket-rule*))))
    (compare-full-match-loosely
     (car result)
     '((:BEGIN 0 :END 16 :MATCH "[2024-01-15 Mon]")
       ((:BEGIN 0 :END 1 :MATCH "["))
       ((:BEGIN 1 :END 5 :ID YEAR :MATCH "2024"))
       ((:BEGIN 5 :END 6 :MATCH "-"))
       ((:BEGIN 6 :END 8 :ID MONTH :MATCH "01"))
       ((:BEGIN 8 :END 9 :MATCH "-"))
       ((:BEGIN 9 :END 11 :ID DAY :MATCH "15"))
       ((:BEGIN 11 :END 12 :MATCH " "))
       ((:BEGIN 12 :END 15 :ID WEEKDAY :MATCH "Mon"))
       ((:BEGIN 15 :END 16 :MATCH "]"))))))

(test org-timestamp-bracket-basic
  (fiveam:is (org-timestamp-bracket-basic-func)))

(defun org-link-simple-func ()
  (let ((result (cltpt/combinator:parse
                 "[[id:abc123]]"
                 (list cltpt/org-mode::*org-link-rule*))))
    (compare-full-match-loosely
     (car result)
     '((:BEGIN 0 :END 13 :MATCH "[[id:abc123]]")
       ((:BEGIN 0 :END 2 :MATCH "[["))
       ((:BEGIN 2 :END 4 :MATCH "id"))
       ((:BEGIN 4 :END 5 :MATCH ":"))
       ((:BEGIN 5 :END 11 :MATCH "abc123"))
       ((:BEGIN 11 :END 13 :MATCH "]]"))))))

(test org-link-simple
  (fiveam:is (org-link-simple-func)))

(defun org-link-with-description-func ()
  (let ((result (cltpt/combinator:parse
                 "[[file:document.pdf][My Document]]"
                 (list cltpt/org-mode::*org-link-rule*))))
    (compare-full-match-loosely
     (car result)
     '((:BEGIN 0 :END 34 :MATCH "[[file:document.pdf][My Document]]")
       ((:BEGIN 0 :END 2 :MATCH "[["))
       ((:BEGIN 2 :END 6 :MATCH "file"))
       ((:BEGIN 6 :END 7 :MATCH ":"))
       ((:BEGIN 7 :END 19 :MATCH "document.pdf"))
       ((:BEGIN 19 :END 21 :MATCH "]["))
       ((:BEGIN 21 :END 32 :MATCH "My Document"))
       ((:BEGIN 32 :END 34 :MATCH "]]"))))))

(test org-link-with-description
  (fiveam:is (org-link-with-description-func)))

(defun org-link-no-type-func ()
  (let ((result (cltpt/combinator:parse
                 "[[document.pdf]]"
                 (list cltpt/org-mode::*org-link-rule*))))
    (compare-full-match-loosely
     (car result)
     '((:BEGIN 0 :END 16 :MATCH "[[document.pdf]]")
       ((:BEGIN 0 :END 2 :MATCH "[["))
       ((:BEGIN 2 :END 14 :MATCH "document.pdf"))
       ((:BEGIN 14 :END 16 :MATCH "]]"))))))

(test org-link-no-type
  (fiveam:is (org-link-no-type-func)))

(defun org-web-link-basic-func ()
  (let ((result (cltpt/combinator:parse
                 "https://example.com/page"
                 (list cltpt/org-mode::*web-link-rule*))))
    (compare-full-match-loosely
     (car result)
     '((:BEGIN 0 :END 24 :MATCH "https://example.com/page")
       ((:BEGIN 0 :END 8 :MATCH "https://"))
       ((:BEGIN 8 :END 24 :MATCH "example.com/page"))))))

(test org-web-link-basic
  (fiveam:is (org-web-link-basic-func)))

(defun org-inline-code-basic-func ()
  (let ((result (cltpt/combinator:parse
                 "~code here~"
                 (list cltpt/org-mode::*org-inline-code-rule*))))
    (compare-full-match-loosely
     (car result)
     '((:BEGIN 0 :END 11 :MATCH "~code here~")
       ((:BEGIN 0 :END 1 :MATCH "~"))
       ((:BEGIN 1 :END 10 :MATCH "code here"))
       ((:BEGIN 10 :END 11 :MATCH "~"))))))

(test org-inline-code-basic
  (fiveam:is (org-inline-code-basic-func)))

(defun org-inline-code-with-spaces-func ()
  (let ((result (cltpt/combinator:parse
                 "~my code here~"
                 (list cltpt/org-mode::*org-inline-code-rule*))))
    (compare-full-match-loosely
     (car result)
     '((:BEGIN 0 :END 14 :MATCH "~my code here~")
       ((:BEGIN 0 :END 1 :MATCH "~"))
       ((:BEGIN 1 :END 13 :MATCH "my code here"))
       ((:BEGIN 13 :END 14 :MATCH "~"))))))

(test org-inline-code-with-spaces
  (fiveam:is (org-inline-code-with-spaces-func)))

(defun org-keywords-basic-func ()
  (let ((result (cltpt/combinator:parse
                 " :hello-there :hello2"
                 (list cltpt/org-mode::*keywords-rule*))))
    (compare-full-match-loosely
     (car result)
     '((:BEGIN 1 :END 18 :ID CLTPT/ORG-MODE::KEYWORDS)
       ((:BEGIN 1 :END 2 :MATCH " "))
       ((:BEGIN 2 :END 13 :ID CLTPT/ORG-MODE::KEYWORD)
        ((:BEGIN 2 :END 3 :MATCH ":"))
        ((:BEGIN 3 :END 13 :MATCH "hello-there")))
       ((:BEGIN 13 :END 14 :MATCH " "))
       ((:BEGIN 14 :END 18 :ID CLTPT/ORG-MODE::KEYWORD)
        ((:BEGIN 14 :END 15 :MATCH ":"))
        ((:BEGIN 15 :END 18 :MATCH "hello2")))
       ((:BEGIN 22 :END 23 :MATCH " "))
       ((:BEGIN 23 :END 30 :MATCH ":hello2")
        ((:BEGIN 23 :END 24 :MATCH ":"))
        ((:BEGIN 24 :END 30 :MATCH "hello2")))))))

(test org-keywords-basic
  (fiveam:is (org-keywords-basic-func)))

(defun org-italic-basic-func ()
  (let ((result (cltpt/combinator:parse
                 "/italic text/"
                 (list cltpt/org-mode::*org-italic-rule*))))
    (compare-full-match-loosely
     (car result)
     '((:BEGIN 0 :END 13 :MATCH "/italic text/")
       ((:BEGIN 0 :END 1 :MATCH "/"))
       ((:BEGIN 12 :END 13 :MATCH "/"))))))

(test org-italic-basic
  (fiveam:is (org-italic-basic-func)))

(defun org-prop-drawer-basic-test-func ()
  (let ((result (cltpt/combinator:parse
                 ":PROPERTIES:
:ID: my-id-123
:CUSTOM_ID: my-custom
:END:"
                 (list cltpt/org-mode::*org-prop-drawer-rule*))))
    (compare-full-match-loosely
     (car result)
     '((:BEGIN 0 :END 44 :ID CLTPT/ORG-MODE::ORG-PROP-DRAWER)
       ((:BEGIN 0 :END 12 :ID CLTPT/ORG-MODE::DRAWER-OPEN-TAG :MATCH ":PROPERTIES:"))
       ((:BEGIN 12 :END 13 :MATCH "\n"))
       ((:BEGIN 13 :END 24 :ID CLTPT/ORG-MODE::DRAWER-ENTRY)
        ((:BEGIN 13 :END 17 :ID CLTPT/ORG-MODE::DRAWER-KEY :MATCH "ID"))
        ((:BEGIN 17 :END 24 :ID CLTPT/ORG-MODE::DRAWER-VALUE :MATCH " my-id-123")))
       ((:BEGIN 24 :END 25 :MATCH "\n"))
       ((:BEGIN 25 :END 41 :ID CLTPT/ORG-MODE::DRAWER-ENTRY)
        ((:BEGIN 25 :END 35 :ID CLTPT/ORG-MODE::DRAWER-KEY :MATCH "CUSTOM_ID"))
        ((:BEGIN 35 :END 41 :ID CLTPT/ORG-MODE::DRAWER-VALUE :MATCH " my-custom")))
       ((:BEGIN 41 :END 42 :MATCH "\n"))
       ((:BEGIN 42 :END 44 :ID CLTPT/ORG-MODE::DRAWER-CLOSE-TAG :MATCH ":END:"))))))

(test org-prop-drawer-basic
  (fiveam:is (org-prop-drawer-basic-test-func)))

(defun org-prop-drawer-empty-test-func ()
  (let ((result (cltpt/combinator:parse
                 ":PROPERTIES:
:END:"
                 (list cltpt/org-mode::*org-prop-drawer-rule*))))
    (compare-full-match-loosely
     (car result)
     '((:BEGIN 0 :END 14 :ID CLTPT/ORG-MODE::ORG-PROP-DRAWER)
       ((:BEGIN 0 :END 12 :ID CLTPT/ORG-MODE::DRAWER-OPEN-TAG :MATCH ":PROPERTIES:"))
       ((:BEGIN 12 :END 13 :MATCH "\n"))
       ((:BEGIN 13 :END 14 :ID CLTPT/ORG-MODE::DRAWER-CLOSE-TAG :MATCH ":END:"))))))

(test org-prop-drawer-empty
  (fiveam:is (org-prop-drawer-empty-test-func)))

(defun org-drawer-basic-func ()
  (let* ((text ":LOGBOOK:
 - Note taken on [2024-01-15 Mon 10:00]
 :END:")
         (result (cltpt/combinator:parse text (list cltpt/org-mode::*org-drawer-rule*))))
    result))

(test org-drawer-basic
  (let ((result (org-drawer-basic-func)))
    (fiveam:is (not (null result)))
    (fiveam:is (listp result))
    (let ((drawer-match (car result)))
      (fiveam:is (not (null drawer-match)))
      (fiveam:is (= (getf (car drawer-match) :BEGIN) 0))
      (fiveam:is (= (getf (car drawer-match) :END) (length text)))
      (fiveam:is (>= (length (cdr drawer-match)) 2)))))

(defun org-src-block-basic-func ()
  (let ((result (cltpt/combinator:parse
                 "#+begin_src python
print('hello')
#+end_src"
                 (list cltpt/org-mode::*org-src-block-rule*))))
    (compare-full-match-loosely
     (car result)
     '((:BEGIN 0 :END 32 :ID CLTPT/ORG-MODE::ORG-SRC-BLOCK)
       ((:BEGIN 0 :END 13 :MATCH "#+begin_src "))
       ((:BEGIN 13 :END 19 :ID LANG :MATCH "python"))
       ((:BEGIN 19 :END 20 :MATCH "
"))
       ((:BEGIN 20 :END 31 :MATCH "print('hello')"))
       ((:BEGIN 31 :END 32 :MATCH "
"))
       ((:BEGIN 32 :END 40 :MATCH "#+end_src"))))))

(test org-src-block-basic
  (fiveam:is (org-src-block-basic-func)))

(defun org-src-block-with-options-func ()
  (let ((result (cltpt/combinator:parse
                 "#+begin_src python :results output :exports both
print('hello')
#+end_src"
                 (list cltpt/org-mode::*org-src-block-rule*))))
    (compare-full-match-loosely
     (car result)
     '((:BEGIN 0 :END 60 :ID CLTPT/ORG-MODE::ORG-SRC-BLOCK)
       ((:BEGIN 0 :END 13 :MATCH "#+begin_src "))
       ((:BEGIN 13 :END 19 :ID LANG :MATCH "python"))
       ((:BEGIN 19 :END 20 :MATCH " "))
       ((:BEGIN 20 :END 44 :MATCH ":results output :exports both"))
       ((:BEGIN 44 :END 45 :MATCH "
"))
       ((:BEGIN 45 :END 56 :MATCH "print('hello')"))
       ((:BEGIN 56 :END 57 :MATCH "
"))
       ((:BEGIN 57 :END 65 :MATCH "#+end_src"))))))

(test org-src-block-with-options
  (fiveam:is (org-src-block-with-options-func)))

(defun org-src-block-with-name-func ()
  (let ((result (cltpt/combinator:parse
                 "#+name: my-code-block
#+begin_src lisp
(+ 1 2)
#+end_src"
                 (list cltpt/org-mode::*org-src-block-rule*))))
    (compare-full-match-loosely
     (car result)
     '((:BEGIN 22 :END 47 :ID CLTPT/ORG-MODE::ORG-SRC-BLOCK)
       ((:BEGIN 22 :END 35 :MATCH "#+begin_src "))
       ((:BEGIN 35 :END 39 :ID LANG :MATCH "lisp"))
       ((:BEGIN 39 :END 40 :MATCH "
"))
       ((:BEGIN 40 :END 46 :MATCH "(+ 1 2)"))
       ((:BEGIN 46 :END 47 :MATCH "
"))
       ((:BEGIN 47 :END 55 :MATCH "#+end_src"))))))

(test org-src-block-with-name
  (fiveam:is (org-src-block-with-name-func)))

(defun org-src-block-comprehensive-with-results-func ()
  (let ((result (cltpt/combinator::parse
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
                  cltpt/org-mode::*org-src-block-rule*))))
    (compare-full-match-loosely
     (car result)
     '((:ID CLTPT/ORG-MODE::ORG-SRC-BLOCK)))))

(test org-src-block-comprehensive-with-results
  (fiveam:is (org-src-block-comprehensive-with-results-func)))

(defun org-src-block-comprehensive-with-file-results-func ()
  (let ((result (cltpt/combinator::parse
                 "
#+begin_src python :results output
  do nothing
#+end_src

#+RESULTS[ca08ab2a6a58662675694033105ab0b331611fa2]:
[[file:~/brain/out/jyBtMrE.svg]]
"
                 (list
                  cltpt/org-mode::*org-src-block-rule*))))
    (compare-full-match-loosely
     (car result)
     '((:ID CLTPT/ORG-MODE::ORG-SRC-BLOCK)))))

(test org-src-block-comprehensive-with-file-results
  (fiveam:is (org-src-block-comprehensive-with-file-results-func)))

(defun org-export-block-html-func ()
  (let ((result (cltpt/combinator:parse
                 "#+begin_export html
<div>Custom HTML</div>
#+end_export"
                 (list cltpt/org-mode::*org-export-block-rule*))))
    (compare-full-match-loosely
     (car result)
     '((:BEGIN 0 :END 46 :ID CLTPT/ORG-MODE::ORG-EXPORT-BLOCK)
       ((:BEGIN 0 :END 19 :MATCH "#+begin_export html"))
       ((:BEGIN 19 :END 20 :MATCH "
"))
       ((:BEGIN 20 :END 37 :MATCH "<div>Custom HTML</div>"))
       ((:BEGIN 37 :END 38 :MATCH "
"))
       ((:BEGIN 38 :END 46 :MATCH "#+end_export"))))))

(test org-export-block-html
  (fiveam:is (org-export-block-html-func)))

(defun org-export-block-latex-func ()
  (let ((result (cltpt/combinator:parse
                 "#+begin_export latex
\\textbf{Bold text}
#+end_export"
                 (list cltpt/org-mode::*org-export-block-rule*))))
    (compare-full-match-loosely
     (car result)
     '((:BEGIN 0 :END 44 :ID CLTPT/ORG-MODE::ORG-EXPORT-BLOCK)
       ((:BEGIN 0 :END 20 :MATCH "#+begin_export latex"))
       ((:BEGIN 20 :END 21 :MATCH "
"))
       ((:BEGIN 21 :END 36 :MATCH "\\textbf{Bold text}"))
       ((:BEGIN 36 :END 37 :MATCH "
"))
       ((:BEGIN 37 :END 44 :MATCH "#+end_export"))))))

(test org-export-block-latex
  (fiveam:is (org-export-block-latex-func)))

(defun org-block-example-func ()
  (let ((result (cltpt/combinator:parse
                 "#+begin_example: test"
                 (list cltpt/org-mode::*org-keyword-rule*))))
    (compare-full-match-loosely
     (car result)
     '((:BEGIN 0 :END 21 :ID CLTPT/ORG-MODE::ORG-KEYWORD)
       ((:BEGIN 0 :END 2 :MATCH "#+"))
       ((:BEGIN 2 :END 16 :ID KEYWORD :MATCH "begin_example"))
       ((:BEGIN 16 :END 17 :MATCH ":"))
       ((:BEGIN 17 :END 18 :MATCH " "))
       ((:BEGIN 18 :END 22 :ID VALUE :MATCH "test"))))))

(test org-block-example
  (fiveam:is (org-block-example-func)))

(defun org-block-with-keywords-func ()
  (let ((result (cltpt/combinator:parse
                 "#+name: my-block"
                 (list cltpt/org-mode::*org-keyword-rule*))))
    (compare-full-match-loosely
     (car result)
     '((:BEGIN 0 :END 14 :ID CLTPT/ORG-MODE::ORG-KEYWORD)
       ((:BEGIN 0 :END 2 :MATCH "#+"))
       ((:BEGIN 2 :END 6 :ID KEYWORD :MATCH "name"))
       ((:BEGIN 6 :END 7 :MATCH ":"))
       ((:BEGIN 7 :END 8 :MATCH " "))
       ((:BEGIN 8 :END 15 :ID VALUE :MATCH "my-block"))))))

(test org-block-with-keywords
  (fiveam:is (org-block-with-keywords-func)))

(defun org-babel-results-simple-func ()
  (let ((result (cltpt/combinator:parse
                 "#+RESULTS:
: 42"
                 (list cltpt/org-mode::*org-babel-results-rule*))))
    (compare-full-match-loosely
     (car result)
     '((:BEGIN 0 :END 12 :ID CLTPT/ORG-MODE::ORG-BABEL-RESULTS)
       ((:BEGIN 0 :END 10 :MATCH "#+RESULTS:
"))
       ((:BEGIN 10 :END 12 :ID CLTPT/ORG-MODE::ORG-TEXT)
        ((:BEGIN 10 :END 12 :MATCH ": 42")))))))

(test org-babel-results-simple
  (fiveam:is (org-babel-results-simple-func)))

(defun org-babel-results-with-hash-func ()
  (let ((result (cltpt/combinator:parse
                 "#+RESULTS[abc123def]:
: output here"
                 (list cltpt/org-mode::*org-babel-results-rule*))))
    (compare-full-match-loosely
     (car result)
     '((:BEGIN 0 :END 28 :ID CLTPT/ORG-MODE::ORG-BABEL-RESULTS)
       ((:BEGIN 0 :END 22 :MATCH "#+RESULTS[abc123def]:
"))
       ((:BEGIN 22 :END 28 :ID CLTPT/ORG-MODE::ORG-TEXT)
        ((:BEGIN 22 :END 28 :MATCH ": output here")))))))

(test org-babel-results-with-hash
  (fiveam:is (org-babel-results-with-hash-func)))

(defun org-babel-results-comprehensive-func ()
  (let ((result (cltpt/combinator::parse
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
                  cltpt/org-mode::*org-babel-results-rule*))))
    (eq (getf (car (car result)) :ID) 'CLTPT/ORG-MODE::ORG-BABEL-RESULTS)))

(test org-babel-results-comprehensive
  (fiveam:is (org-babel-results-comprehensive-func)))

(defun org-latex-env-basic-func ()
  (let ((result (cltpt/combinator:parse
                 "#+latex: \\begin{equation}"
                 (list cltpt/org-mode::*org-keyword-rule*))))
    (compare-full-match-loosely
     (car result)
     '((:BEGIN 0 :END 26 :ID CLTPT/ORG-MODE::ORG-KEYWORD)
       ((:BEGIN 0 :END 2 :MATCH "#+"))
       ((:BEGIN 2 :END 7 :ID KEYWORD :MATCH "latex"))
       ((:BEGIN 7 :END 8 :MATCH ":"))
       ((:BEGIN 8 :END 9 :MATCH " "))
       ((:BEGIN 9 :END 26 :ID VALUE :MATCH "\\begin{equation}"))))))

(test org-latex-env-basic
  (fiveam:is (org-latex-env-basic-func)))

(defun org-latex-env-with-name-func ()
  (let ((result (cltpt/combinator:parse
                 "#+name: eq1"
                 (list cltpt/org-mode::*org-keyword-rule*))))
    (compare-full-match-loosely
     (car result)
     '((:BEGIN 0 :END 12 :ID CLTPT/ORG-MODE::ORG-KEYWORD)
       ((:BEGIN 0 :END 2 :MATCH "#+"))
       ((:BEGIN 2 :END 6 :ID KEYWORD :MATCH "name"))
       ((:BEGIN 6 :END 7 :MATCH ":"))
       ((:BEGIN 7 :END 8 :MATCH " "))
       ((:BEGIN 8 :END 11 :ID VALUE :MATCH "eq1"))))))

(test org-latex-env-with-name
  (fiveam:is (org-latex-env-with-name-func)))

(defun latex-env-parse-test-1 ()
  (cltpt/combinator::parse
   "
\\\\begin{gather}
some math here
\\end{gather}
"
   (list
    cltpt/latex::*latex-env-rule*)))

(test latex-env-parse-test-1
  (fiveam:is
   (compare-full-match-loosely
    (car (latex-env-parse-test-1))
    '((:BEGIN 1 :END 43 :MATCH "\\begin{gather}
some math here
\\end{gather}")
      ((:BEGIN 1 :END 15 :ID CLTPT/LATEX::OPEN-TAG :MATCH "\\begin{gather}")
       ((:BEGIN 1 :END 8 :MATCH "\\begin{"))
       ((:BEGIN 8 :END 14 :MATCH "gather"))
       ((:BEGIN 14 :END 15 :MATCH "}")))
      ((:BEGIN 31 :END 43 :ID CLTPT/LATEX::CLOSE-TAG :MATCH "\\end{gather}")
       ((:BEGIN 31 :END 36 :MATCH "\\end{"))
       ((:BEGIN 36 :END 42 :MATCH "gather"))
       ((:BEGIN 42 :END 43 :MATCH "}")))))))

(defun inline-latex-test-func ()
  (let ((other-rules
          `((:pattern ,(cltpt/combinator:compile-rule-string "%w world")
             :id keyword))))
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
  (is (compare-full-match-loosely
       (car (cltpt/tests/org-mode::inline-latex-test-func))
       '((:BEGIN 6 :END 26)
         ((:ID CLTPT/TESTS/ORG-MODE::OPENING :BEGIN 6 :END 8))
         ((:ID KEYWORD :BEGIN 13 :END 24)
          ((:BEGIN 13 :END 18)) ((:BEGIN 18 :END 24)))
         ((:ID CLTPT/TESTS/ORG-MODE::ENDING :BEGIN 24 :END 26))))))

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
   (compare-full-match-loosely
    (org-list-test-1-func)
    '((:ID CLTPT/ORG-MODE:ORG-LIST :BEGIN 0 :END 97 :INDENT 0)
      ((:ID CLTPT/ORG-MODE::LIST-ITEM :INDENT 0 :BEGIN 0 :END 85)
       ((:ID CLTPT/ORG-MODE::LIST-ITEM-BULLET :BEGIN 0 :END 2))
       ((:ID CLTPT/ORG-MODE::LIST-ITEM-CONTENT :BEGIN 2 :END 18)
        ((:BEGIN 10 :END 17 :ID CLTPT/TESTS::MYPAIR)
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
     text
     0
     '((:pattern (cltpt/combinator::literal "item") :id item-keyword)
       (:pattern (cltpt/combinator::literal "nested") :id nested-keyword)
       (:pattern (cltpt/combinator::word-matcher) :id generic-word)))))

(test org-list-test-2
  (is
   (compare-full-match-loosely
    (org-list-test-2-func)
    '((:ID CLTPT/ORG-MODE:ORG-LIST :BEGIN 0 :END 151 :INDENT 0)
      ((:ID CLTPT/ORG-MODE::LIST-ITEM :INDENT 0 :BEGIN 0 :END 32)
       ((:ID CLTPT/ORG-MODE::LIST-ITEM-BULLET :BEGIN 0 :END 2))
       ((:ID CLTPT/ORG-MODE::LIST-ITEM-CONTENT :BEGIN 2 :END 32)
        ((:BEGIN 2 :END 6 :ID CLTPT/TESTS::ITEM-KEYWORD))
        ((:BEGIN 7 :END 10 :ID CLTPT/TESTS::GENERIC-WORD))
        ((:BEGIN 11 :END 16 :ID CLTPT/TESTS::GENERIC-WORD))
        ((:BEGIN 17 :END 21 :ID CLTPT/TESTS::GENERIC-WORD))
        ((:BEGIN 22 :END 25 :ID CLTPT/TESTS::GENERIC-WORD))
        ((:BEGIN 26 :END 29 :ID CLTPT/TESTS::GENERIC-WORD))))
      ((:ID CLTPT/ORG-MODE::LIST-ITEM :INDENT 0 :BEGIN 32 :END 139)
       ((:ID CLTPT/ORG-MODE::LIST-ITEM-BULLET :BEGIN 32 :END 34))
       ((:ID CLTPT/ORG-MODE::LIST-ITEM-CONTENT :BEGIN 34 :END 50)
        ((:BEGIN 34 :END 36 :ID CLTPT/TESTS::GENERIC-WORD))
        ((:BEGIN 37 :END 41 :ID CLTPT/TESTS::GENERIC-WORD))
        ((:BEGIN 44 :END 45 :ID CLTPT/TESTS::GENERIC-WORD))
        ((:BEGIN 46 :END 47 :ID CLTPT/TESTS::GENERIC-WORD))
        ((:ID CLTPT/ORG-MODE:ORG-LIST :BEGIN 50 :END 139 :INDENT 2)
         ((:ID CLTPT/ORG-MODE::LIST-ITEM :INDENT 2 :BEGIN 50 :END 118)
          ((:ID CLTPT/ORG-MODE::LIST-ITEM-BULLET :BEGIN 52 :END 55))
          ((:ID CLTPT/ORG-MODE::LIST-ITEM-CONTENT :BEGIN 55 :END 93)
           ((:BEGIN 55 :END 61 :ID CLTPT/TESTS::NESTED-KEYWORD))
           ((:BEGIN 62 :END 66 :ID CLTPT/TESTS::ITEM-KEYWORD))
           ((:BEGIN 67 :END 70 :ID CLTPT/TESTS::GENERIC-WORD))
           ((:BEGIN 71 :END 75 :ID CLTPT/TESTS::GENERIC-WORD))
           ((:BEGIN 76 :END 82 :ID CLTPT/TESTS::NESTED-KEYWORD))
           ((:BEGIN 83 :END 87 :ID CLTPT/TESTS::GENERIC-WORD))
           ((:ID CLTPT/ORG-MODE:ORG-LIST :BEGIN 93 :END 118 :INDENT 5)
            ((:ID CLTPT/ORG-MODE::LIST-ITEM :INDENT 5 :BEGIN 93 :END 118)
             ((:ID CLTPT/ORG-MODE::LIST-ITEM-BULLET :BEGIN 98 :END 101))
             ((:ID CLTPT/ORG-MODE::LIST-ITEM-CONTENT :BEGIN 101 :END 118)
              ((:BEGIN 101 :END 105 :ID CLTPT/TESTS::GENERIC-WORD))
              ((:BEGIN 106 :END 110 :ID CLTPT/TESTS::GENERIC-WORD))
              ((:BEGIN 111 :END 117 :ID CLTPT/TESTS::NESTED-KEYWORD)))))))
         ((:ID CLTPT/ORG-MODE::LIST-ITEM :INDENT 2 :BEGIN 118 :END 139)
          ((:ID CLTPT/ORG-MODE::LIST-ITEM-BULLET :BEGIN 120 :END 123))
          ((:ID CLTPT/ORG-MODE::LIST-ITEM-CONTENT :BEGIN 123 :END 139)
           ((:BEGIN 123 :END 129 :ID CLTPT/TESTS::NESTED-KEYWORD))
           ((:BEGIN 130 :END 134 :ID CLTPT/TESTS::ITEM-KEYWORD))
           ((:BEGIN 135 :END 138 :ID CLTPT/TESTS::GENERIC-WORD)))))))
      ((:ID CLTPT/ORG-MODE::LIST-ITEM :INDENT 0 :BEGIN 139 :END 151)
       ((:ID CLTPT/ORG-MODE::LIST-ITEM-BULLET :BEGIN 139 :END 141))
       ((:ID CLTPT/ORG-MODE::LIST-ITEM-CONTENT :BEGIN 141 :END 151)
        ((:BEGIN 141 :END 145 :ID CLTPT/TESTS::ITEM-KEYWORD))
        ((:BEGIN 146 :END 151 :ID CLTPT/TESTS::GENERIC-WORD))))))))

(defun org-list-test-3-func ()
  (let* ((text "- we have \\(x=y\\)
   a. nested item one
      more nested text
      1. test1
      2. test2
   b. nested item two
- item three")
         (parsed-list (cltpt/org-mode::org-list-matcher nil text 0))
         (html-output (cltpt/org-mode::to-html-list parsed-list)))
    ;; (format t "html output: ~S~%" html-output)
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
<li>test1
</li>
<li>test2
</li>
</ol>
</li>
<li>nested item two
</li>
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
         (parsed-list (cltpt/org-mode::org-list-matcher nil text 0))
         (latex-output (cltpt/org-mode::to-latex-list parsed-list)))
    latex-output))

(test org-list-test-4
  (let ((result (org-list-test-4-func)))
    (fiveam:is (stringp result))
    (fiveam:is (> (length result) 0))
    (fiveam:is (search "\\begin{itemize}" result))
    (fiveam:is (search "\\end{itemize}" result))))

(defun org-list-test-5-func ()
  (let* ((text "- we have [[mylink]]
   a. nested item one \\(x=y\\)
      more nested text
      1. test1
      2. test2
   b. nested item two
- item three"))
    (cltpt/base:parse
     cltpt/org-mode:*org-mode*
     text
     :text-object-types (list 'cltpt/org-mode::org-list 'cltpt/org-mode::org-link))))

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
     text
     0
     (cltpt/org-mode::org-mode-inline-text-object-rule))))

(test org-list-test-6
  (let ((result (org-list-test-6-func)))
    (fiveam:is (not (null result)))
    (fiveam:is (listp result))
    (fiveam:is (eq (getf (car result) :ID) 'CLTPT/ORG-MODE:ORG-LIST))))

(defun test-parse-table-func ()
  (let ((table
          "| head1 | head2 | head3 |
+------+-------+-------+
|  foo |  bar  |  baz  |
| 123  | 456   | 789   |
+------+-------+-------+
| end  | row   | test  |"))
    (equal
     (org-table-parse table)
     '(("head1" "head2" "head3")
       ("foo" "bar" "baz")
       ("123" "456" "789")
       ("end" "row" "test")))))

(test test-parse-table
  (fiveam:is (test-parse-table-func)))

(defun test-parse-any-func ()
  (cltpt/combinator:parse
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
  (is (compare-full-match-loosely
       (car (test-parse-any-func))
       '((:ID CLTPT/TESTS::ORG-LINK :BEGIN 0 :END 18)
         ((:BEGIN 0 :END 2))
         ((:ID CLTPT/TESTS::TEST2 :BEGIN 2 :END 7))
         ((:BEGIN 7 :END 8))
         ((:ID CLTPT/TESTS::TEST3 :BEGIN 8 :END 11))
         ((:BEGIN 11 :END 13))
         ((:BEGIN 13 :END 16))
         ((:BEGIN 16 :END 18))))))

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

(defun test-org-table-1-func ()
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

;; TODO: this test needs more detailed verification of the table structure and cell contents
(test test-org-table-1
  (let ((result (test-org-table-1-func)))
    (fiveam:is
     (and result
          (listp result)
          (eq (getf (car result) :ID) 'CLTPT/ORG-MODE::ORG-TABLE)
          (> (length result) 1)))))

(defun test-org-table-2 ()
  (let* ((misaligned-table-text
          "| name | age|
|------+----|
|alice|  25 |
|  bob |30 |
|      |    |
| charlie| 9 |")
         (parse-tree (cltpt/org-mode::org-table-matcher nil misaligned-table-text 0)))
    (when parse-tree
      (let ((formatted-table-text (cltpt/org-mode::reformat-table parse-tree)))
        formatted-table-text))))

(defun test-org-table-2-func ()
  (let* ((misaligned-table-text
          "| name | age|
|------+----|
|alice|  25 |
|  bob |30 |
|      |    |
| charlie| 9 |")
         (parse-tree (cltpt/org-mode::org-table-matcher nil misaligned-table-text 0)))
    (when parse-tree
      (cltpt/org-mode::reformat-table parse-tree))))

(test test-org-table-2
  (let ((result (test-org-table-2-func)))
    (fiveam:is (stringp result))
    (fiveam:is (search "name" result))
    (fiveam:is (search "alice" result))
    (fiveam:is (search "charlie" result))))

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
      nil
      text
      0))))

(defun test-org-table-3-func ()
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
      nil
      text
      0))))

(test test-org-table-3
  (let ((result (test-org-table-3-func)))
    (fiveam:is (stringp result))
    (fiveam:is (search "<table>" result))
    (fiveam:is (search "</table>" result))
    (fiveam:is (search "head1" result))))

(defun test-pairs-1-func ()
  (let* ((other-rules `((:pattern ,(cltpt/combinator:compile-rule-string "#+%w")
                         :id keyword)))
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
     (compare-full-match-loosely
      (car parser-result)
      '((:BEGIN 0 :END 36)
        ((:ID OPENING :BEGIN 0 :END 1))
        ((:ID KEYWORD :BEGIN 25 :END 33)
         ((:BEGIN 25 :END 27)) ((:BEGIN 27 :END 33)))
        ((:ID ENDING :BEGIN 35 :END 36)))))))

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
   (compare-full-match-loosely
    (car (cltpt/tests/org-mode::test-sharp-lisp-1-func))
    '((:ID CLTPT/TESTS/ORG-MODE::SHARP-LISP-BLOCK :BEGIN 0 :END 29)
      ((:BEGIN 0 :END 1))
      ((:ID CLTPT/TESTS/ORG-MODE::LISP-CODE :BEGIN 1 :END 29
        :LISP-FORM (FORMAT T "hello)(\" there")
        :ID CLTPT/COMBINATOR::LISP-FORM-CONTENT))))))

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

(test test-parse-escape
  (let ((result (test-parse-escape-func)))
    (fiveam:is (= (length result) 2))
    (fiveam:is (compare-full-match-loosely
                (car result)
                '((:ID CLTPT/TESTS::ORG-LINK :BEGIN 32 :END 50)
                  ((:BEGIN 32 :END 34))
                  ((:ID CLTPT/TESTS::TEST2 :BEGIN 34 :END 39))
                  ((:BEGIN 39 :END 40))
                  ((:ID CLTPT/TESTS::TEST3 :BEGIN 40 :END 43))
                  ((:BEGIN 43 :END 45))
                  ((:BEGIN 45 :END 48))
                  ((:BEGIN 48 :END 50)))))
    (fiveam:is (compare-full-match-loosely
                (cadr result)
                '((:ID CLTPT/TESTS::ORG-LINK :BEGIN 72 :END 90)
                  ((:BEGIN 72 :END 74))
                  ((:ID CLTPT/TESTS::TEST2 :BEGIN 74 :END 77))
                  ((:BEGIN 77 :END 78))
                  ((:ID CLTPT/TESTS::TEST3 :BEGIN 78 :END 81))
                  ((:BEGIN 81 :END 83))
                  ((:BEGIN 83 :END 87))
                  ((:BEGIN 87 :END 89)))))))

(defun test-pairs-2-func ()
  (let* ((other-rules `((:pattern ,(cltpt/combinator:compile-rule-string "#+%w")
                         :id keyword)))
         (rules
           `((cltpt/combinator::pair
              (:pattern (cltpt/combinator::unescaped (cltpt/combinator::literal "*"))
               :id openingg)
              (:pattern (cltpt/combinator::literal "*")
               :id endingg)
              ,other-rules
              nil
              nil))))
    (cltpt/combinator:scan-all-rules
     nil
     "\\**my text #+here* *hello there* *more
here* here"
     rules)))

(test test-pairs-2
  (let ((result (test-pairs-2-func)))
    (fiveam:is (= (length result) 2))
    (fiveam:is (compare-full-match-loosely
                (car result)
                '((:ID OPENINGG :BEGIN 2 :END 17)
                  ((:ID OPENINGG :BEGIN 2 :END 3))
                  ((:ID CLTPT/TESTS/ORG-MODE::KEYWORD :BEGIN 12 :END 19))
                  ((:ID CLTPT/TESTS/ORG-MODE::ENDINGG :BEGIN 18 :END 19)))))))

(defun test-pairs-3-func ()
  (let* ((other-rules
           `((:pattern
              ,(cltpt/combinator::compile-rule-string "#%W")
              :id keyword)))
         (rules
           `((cltpt/combinator::pair
              (:pattern (cltpt/combinator::unescaped
                         (cltpt/combinator::literal "*"))
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

(test test-pairs-3
  (let ((result (test-pairs-3-func)))
    (fiveam:is (= (length result) 2))
    (fiveam:is (compare-full-match-loosely
                (car result)
                '((:ID OPENINGG :BEGIN 2 :END 17)
                  ((:ID OPENINGG :BEGIN 2 :END 3))
                  ((:ID CLTPT/TESTS/ORG-MODE::KEYWORD :BEGIN 12 :END 19))
                  ((:ID CLTPT/TESTS/ORG-MODE::ENDINGG :BEGIN 18 :END 19)))))))

(defun test-eol ()
  (let* ((rule1 `(:pattern
                  (cltpt/combinator::followed-by
                   (:pattern ,(cltpt/combinator::compile-rule-string "#%W")
                    :id hashtag)
                   cltpt/combinator:at-line-end-p)
                  :id eol-tag)))
    (cltpt/combinator::scan-all-rules
     nil
     "#tag1
a #tag2 in the middle
and a final #tag3"
     (list rule1))))

(defun test-eol-func ()
  (let* ((rule1 `(:pattern
                  (cltpt/combinator::followed-by
                   (:pattern ,(cltpt/combinator::compile-rule-string "#%W")
                    :id hashtag)
                   cltpt/combinator:at-line-end-p)
                  :id eol-tag)))
    (cltpt/combinator::scan-all-rules
     nil
     "#tag1
 a #tag2 in the middle
 and a final #tag3"
     (list rule1))))

(test test-eol
  (let ((result (test-eol-func)))
    (fiveam:is (= (length result) 2))
    (fiveam:is (eq (getf (car (car result)) :ID) 'EOL-TAG))
    (fiveam:is (eq (getf (car (cadr result)) :ID) 'EOL-TAG))))

(defun test-bol ()
  (let* ((rule1 `(:pattern
                  (cltpt/combinator::when-match
                   (:pattern ,(cltpt/combinator::compile-rule-string "#%W")
                    :id hashtag)
                   cltpt/combinator:at-line-start-p)
                  :id bol-tag)))
    (cltpt/combinator::scan-all-rules
     nil
     "#tag1 is on line 1
 this is not a match: #tag2
#tag3 is on line 3"
     (list rule1))))

(defun test-bol-extended-test-fn ()
  (let ((result (test-bol)))
    (and (= (length result) 2)
         (compare-full-match-loosely
          (car result)
          '((:ID BOL-TAG :BEGIN 0 :END 5)
            ((:BEGIN 0 :END 5 :ID HASHTAG :MATCH "#tag1"))))
         (compare-full-match-loosely
          (cadr result)
          '((:ID BOL-TAG :BEGIN 25 :END 30)
            ((:BEGIN 25 :END 30 :ID HASHTAG :MATCH "#tag3")))))))

(test test-bol
  (fiveam:is (test-bol-extended-test-fn)))

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
    (cltpt/base:parse cltpt/org-mode:*org-mode* text)))

(defun keywords-test-1 ()
  (cltpt/combinator:parse
   " :hello-there test :hello2 test2"
   (list cltpt/org-mode::*keywords-rule*)))

(test keywords-test-1
  (fiveam:is
   (compare-full-match-loosely
    (car (keywords-test-1))
    '((:BEGIN 1 :END 32 :ID CLTPT/ORG-MODE::KEYWORDS :MATCH ":hello-there test :hello2 test2")
      ((:BEGIN 1 :END 18 :ID CLTPT/ORG-MODE::KEYWORDS-ENTRY :MATCH ":hello-there test")
       ((:BEGIN 1 :END 13 :MATCH ":hello-there")
        ((:BEGIN 1 :END 2 :MATCH ":"))
        ((:BEGIN 2 :END 13 :ID KEYWORD :MATCH "hello-there")))
       ((:BEGIN 13 :END 18 :MATCH " test")
        ((:BEGIN 13 :END 14 :MATCH " "))
        ((:BEGIN 14 :END 18 :ID CLTPT/ORG-MODE::VALUE :MATCH "test"))))
      ((:BEGIN 18 :END 19 :MATCH " "))
      ((:BEGIN 19 :END 32 :ID CLTPT/ORG-MODE::KEYWORDS-ENTRY :MATCH ":hello2 test2")
       ((:BEGIN 19 :END 26 :MATCH ":hello2")
        ((:BEGIN 19 :END 20 :MATCH ":"))
        ((:BEGIN 20 :END 26 :ID KEYWORD :MATCH "hello2")))
       ((:BEGIN 26 :END 32 :MATCH " test2")
        ((:BEGIN 26 :END 27 :MATCH " "))
        ((:BEGIN 27 :END 32 :ID CLTPT/ORG-MODE::VALUE :MATCH "test2"))))))))

(defun test-separated-atleast-one ()
  (cltpt/combinator:parse
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

(test test-separated-atleast-one
  (let ((parser-result (test-separated-atleast-one)))
    (fiveam:is
     (compare-full-match-loosely
      (car parser-result)
      '((:BEGIN 2 :END 16)
        ((:BEGIN 2 :END 3))
        ((:BEGIN 3 :END 4))
        ((:ID TAG :BEGIN 4 :END 15)
         ((:BEGIN 4 :END 9 :ID TAG))
         ((:BEGIN 10 :END 15 :ID TAG)))
        ((:BEGIN 15 :END 16)))))))

(defun test-org-latex-env ()
  (cltpt/combinator:parse
   "
#+name: test-name
\\begin{equation}
my equation here
\\end{equation}
"
   (list cltpt/org-mode::*org-latex-env-rule*)))

(test test-org-latex-env
  (fiveam:is
   (compare-full-match-loosely
    (car (test-org-latex-env))
    '((:BEGIN 1 :END 67 :MATCH "#+name: test-name
\\begin{equation}
my equation here
\\end{equation}")
      ((:BEGIN 1 :END 18 :MATCH "#+name: test-name")
       ((:BEGIN 1 :END 18 :ID CLTPT/ORG-MODE::ORG-KEYWORD :MATCH "#+name: test-name")
        ((:BEGIN 1 :END 8 :MATCH "#+name:")
         ((:BEGIN 1 :END 3 :MATCH "#+"))
         ((:BEGIN 3 :END 7 :ID KEYWORD :MATCH "name"))
         ((:BEGIN 7 :END 8 :MATCH ":")))
        ((:BEGIN 8 :END 9 :MATCH " "))
        ((:BEGIN 9 :END 18 :ID CLTPT/ORG-MODE::VALUE :MATCH "test-name"))))
      ((:BEGIN 18 :END 19 :MATCH "
"))
      ((:BEGIN 19 :END 67 :ID CLTPT/ORG-MODE::LATEX-ENV-1 :MATCH "\\begin{equation}
my equation here
\\end{equation}")
       ((:BEGIN 19 :END 35 :ID CLTPT/LATEX::OPEN-TAG :MATCH "\\begin{equation}")
        ((:BEGIN 19 :END 26 :MATCH "\\begin{"))
        ((:BEGIN 26 :END 34 :MATCH "equation"))
        ((:BEGIN 34 :END 35 :MATCH "}")))
       ((:BEGIN 53 :END 67 :ID CLTPT/LATEX::CLOSE-TAG :MATCH "\\end{equation}")
        ((:BEGIN 53 :END 58 :MATCH "\\end{"))
        ((:BEGIN 58 :END 66 :MATCH "equation"))
        ((:BEGIN 66 :END 67 :MATCH "}"))))))))

(defun test-org-latex-env-1 ()
  (cltpt/base:parse
   cltpt/org-mode:*org-mode*
   "
#+name: test-name
\\begin{equation}
my equation here
\\end{equation}
"))

(defun test-org-keyword ()
  (cltpt/combinator:parse
   "
 #+title: add vid to github readme
 #+date: <2024-04-04 Thu 15:52:09>
 #+filetags: 
 #+identifier: 1712235129
 "
   (list cltpt/org-mode::*org-keyword-rule*)))

(test test-org-keyword
  (let ((parser-result (test-org-keyword)))
    (fiveam:is
     (compare-full-match-loosely
      (car parser-result)
      '((:BEGIN 1 :END 34 :ID CLTPT/ORG-MODE::ORG-KEYWORD)
        ((:BEGIN 1 :END 8))
        ((:ID KEYWORD :BEGIN 4 :END 9))
        ((:BEGIN 9 :END 10))
        ((:ID VALUE :BEGIN 10 :END 34)))))))

(defun test-duration-1 ()
  (let ((duration '(:hour 1 :minute 30)))
    (format t "1. ~A~%" (cltpt/base:add-duration (local-time:now) duration))
    (format t "2. ~A~%" (cltpt/base:add-duration (local-time:now) duration :sign -1))))

(defun test-duration-2 ()
  (cltpt/base:list-date-pairs
   (local-time:today)
   (cltpt/base:add-duration (local-time:today) '(:day 14))
   '(:minute 70)))

(defun test-org-duration-parsing-1 ()
  (cltpt/org-mode::get-repeated-duration "1" "w"))

(defun test-org-duration-parsing-2 ()
  (cltpt/org-mode::get-repeated-duration "2" "d"))

(defun test-org-duration-parsing-3 ()
  (cltpt/org-mode::get-repeated-duration "3" "h"))

(test test-org-duration-parsing-1
  (let ((result (test-org-duration-parsing-1)))
    (fiveam:is
     (equal result '(:week 1)))))

(test test-org-duration-parsing-2
  (let ((result (test-org-duration-parsing-2)))
    (fiveam:is
     (equal result '(:day 2)))))

(test test-org-duration-parsing-3
  (let ((result (test-org-duration-parsing-3)))
    (fiveam:is
     (equal result '(:hour 3)))))

(defun run-org-mode-tests ()
  "Run all org-mode rules tests."
  (format t "~&Running org-mode tests...~%")
  (let ((results (run! 'org-mode-suite)))
    (unless results
      (explain! results))))