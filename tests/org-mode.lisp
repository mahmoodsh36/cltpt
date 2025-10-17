(defpackage :cltpt/tests/org-mode
  (:use :cl :it.bese.fiveam))

(in-package :cltpt/tests/org-mode)

(def-suite org-mode-suite
  :description "tests for org-mode.")

(in-suite org-mode-suite)

(defun org-rules ()
  (remove-if-not
   'identity
   (loop
     for type1
       in (cltpt/base:text-format-text-object-types
           cltpt/org-mode:*org-mode*)
     collect (cltpt/base:text-object-rule-from-subclass type1))))

(defun make-dummy-context ()
  (let ((rules (org-rules)))
    (cltpt/combinator::make-context-from-rules rules)))

(defun org-table-parse (table-text)
  "parse an org-mode table and return a list of rows, each row being a list of cell values."
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
  (labels ((my-simplify (m)
             (simplify-match (car m))))
    (cltpt/tree:tree-map match #'my-simplify)))

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
    result))

(test org-header-basic
  (let ((result (org-header-basic-func)))
    (fiveam:is
     (compare-full-match-loosely
      (car result)
      '((:BEGIN 0 :END 11 :STR "* my header")
        ((:ID CLTPT/ORG-MODE::STARS :BEGIN 0 :END 1))
        ((:BEGIN 1 :END 2))
        ((:ID CLTPT/ORG-MODE::TITLE :BEGIN 2 :END 11)))))))

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
  (let ((result (cltpt/combinator:scan-all-rules
                 (make-dummy-context)
                 "* TODO my main header :test:here:noexport:
SCHEDULED: <2024-10-29 Tue 16:41:04>
CLOSED: [2024-10-29 Tue 16:41:03]
<2025-07-25 Fri 10:00:00>
:PROPERTIES:
:ID: my-id
:LAST_REPEAT: [2024-10-29 Tue 16:40:36]
:END:"
                 (list cltpt/org-mode::*org-header-rule*))))
    result))

(test org-header-comprehensive
  (fiveam:is
   (compare-full-match-loosely
    (car (org-header-comprehensive-test-func))
    '((:BEGIN 0 :END 137 :ID CLTPT/ORG-MODE::ORG-HEADER)
      ((:BEGIN 0 :END 1 :ID STARS :MATCH "*"))
      ((:BEGIN 1 :END 2 :MATCH " "))
      ((:BEGIN 2 :END 6 :ID TODO :MATCH "TODO"))
      ((:BEGIN 6 :END 7 :MATCH " "))
      ((:BEGIN 7 :END 23 :ID TITLE :MATCH "my main header "))
      ((:BEGIN 23 :END 39 :ID TAGS :MATCH ":test:here:noexport:"))
      ;; More children for the schedule, closed, properties, etc.
      ))))

(defun org-timestamp-date-only-func ()
  (let ((result (cltpt/combinator:parse
                 "<2024-01-15 Mon>"
                 (list cltpt/org-mode::*org-timestamp-rule*))))
    result))

(test org-timestamp-date-only
  (fiveam:is
   (compare-full-match-loosely
    (car (org-timestamp-date-only-func))
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
    result))

(test org-timestamp-with-repeater
  (fiveam:is
   (compare-full-match-loosely
    (car (org-timestamp-with-repeater-func))
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
  (let ((result (cltpt/combinator:scan-all-rules
                 nil
                 "[2024-01-15 Mon]"
                 (list cltpt/org-mode::*org-timestamp-bracket-rule*))))
    result))

(test org-timestamp-bracket-basic
  (fiveam:is
   (compare-full-match-loosely
    (car (org-timestamp-bracket-basic-func))
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
         (result (cltpt/combinator:scan-all-rules
                  nil
                  text
                  (org-rules))))
    result))

(test org-drawer-basic
  (fiveam:is
   (compare-full-match-loosely
    (car (org-drawer-basic-func))
    '((:MATCH ":LOGBOOK:
- Note taken on [2024-01-15 Mon 10:00]
:END:"
      :END 54 :BEGIN 0)))))

(defun org-src-block-basic-func ()
  (let ((result (cltpt/combinator:parse
                 "#+begin_src python
  print('hello')
#+end_src"
                 (list cltpt/org-mode::*org-src-block-rule*))))
    result))

(test org-src-block-basic
  (fiveam:is
   (compare-full-match-loosely
    (car (org-src-block-basic-func))
    '((:BEGIN 0 :END 43 :STR "#+begin_src python
print('hello')
#+end_src")
      ((:ID CLTPT/ORG-MODE::BEGIN :BEGIN 0 :END 18)
       ((:ID CLTPT/ORG-MODE::OPEN-TAG :BEGIN 0 :END 11))
       ((:BEGIN 11 :END 12))
       ((:ID CLTPT/ORG-MODE::LANG :BEGIN 12 :END 18)))
      ((:ID CLTPT/ORG-MODE::END :BEGIN 34 :END 43))))))

(defun org-src-block-with-options-func ()
  (let ((result (cltpt/combinator:parse
                 "#+begin_src python :results output :exports both
print('hello')
#+end_src"
                 (list cltpt/org-mode::*org-src-block-rule*))))
    result))

(test org-src-block-with-options
  (fiveam:is
   (compare-full-match-loosely
    (car (org-src-block-with-options-func))
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

(defun org-src-block-with-name-func ()
  (let ((result (cltpt/combinator:parse
                 "#+name: my-code-block
#+begin_src lisp
(+ 1 2)
#+end_src"
                 (list cltpt/org-mode::*org-src-block-rule*))))
    result))

(test org-src-block-with-name
  (fiveam:is
   (compare-full-match-loosely
    (car (org-src-block-with-name-func))
    '((:BEGIN 22 :END 47 :ID CLTPT/ORG-MODE::ORG-SRC-BLOCK)
      ((:BEGIN 22 :END 35 :MATCH "#+begin_src "))
      ((:BEGIN 35 :END 39 :ID LANG :MATCH "lisp"))
      ((:BEGIN 39 :END 40 :MATCH "
"))
      ((:BEGIN 40 :END 46 :MATCH "(+ 1 2)"))
      ((:BEGIN 46 :END 47 :MATCH "
"))
      ((:BEGIN 47 :END 55 :MATCH "#+end_src"))))))

(defun org-src-block-comprehensive-with-results-func ()
  (let ((result (cltpt/combinator:scan-all-rules
                 nil
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
                 (org-rules))))
    result))

(test org-src-block-comprehensive-with-results
  (fiveam:is
   (compare-full-match-loosely
    (car (org-src-block-comprehensive-with-results-func))
    '((:MATCH "#+begin_src python :results output
  import requests
  print('whatever')
  print('whatever2')
#+end_src

#+RESULTS:
: whatever
: whatever2
: (11)
: wow"
      :END 152 :BEGIN 1)))))

(defun org-src-block-comprehensive-with-file-results-func ()
  (let ((result (cltpt/combinator::parse
                 "
#+begin_src python :results output
  do nothing
#+end_src

#+RESULTS[ca08ab2a6a58662675694033105ab0b331611fa2]:
[[file:~/brain/out/jyBtMrE.svg]]
"
                 (org-rules))))
    result))

(test org-src-block-comprehensive-with-file-results
  (fiveam:is
   (compare-full-match-loosely
    (car (org-src-block-comprehensive-with-file-results-func))
    '((:ID CLTPT/ORG-MODE::ORG-SRC-BLOCK)))))

(defun org-export-block-html-func ()
  (let ((result (cltpt/combinator:parse
                 "#+begin_export html
<div>Custom HTML</div>
#+end_export"
                 (list cltpt/org-mode::*org-export-block-rule*))))
    result))

(test org-export-block-html
  (fiveam:is
   (compare-full-match-loosely
    (car (org-export-block-html-func))
    '((:BEGIN 0 :END 46 :ID CLTPT/ORG-MODE::ORG-EXPORT-BLOCK)
      ((:BEGIN 0 :END 19 :MATCH "#+begin_export html"))
      ((:BEGIN 19 :END 20 :MATCH "
"))
      ((:BEGIN 20 :END 37 :MATCH "<div>Custom HTML</div>"))
      ((:BEGIN 37 :END 38 :MATCH "
"))
      ((:BEGIN 38 :END 46 :MATCH "#+end_export"))))))

(defun org-export-block-latex-func ()
  (let ((result (cltpt/combinator:parse
                 "#+begin_export latex
\\textbf{Bold text}
#+end_export"
                 (list cltpt/org-mode::*org-export-block-rule*))))
    result))

(test org-export-block-latex
  (fiveam:is
   (compare-full-match-loosely
    (car (org-export-block-latex-func))
    '((:BEGIN 0 :END 44 :ID CLTPT/ORG-MODE::ORG-EXPORT-BLOCK)
      ((:BEGIN 0 :END 20 :MATCH "#+begin_export latex"))
      ((:BEGIN 20 :END 21 :MATCH "
"))
      ((:BEGIN 21 :END 36 :MATCH "\\textbf{Bold text}"))
      ((:BEGIN 36 :END 37 :MATCH "
"))
      ((:BEGIN 37 :END 44 :MATCH "#+end_export"))))))

(defun org-block-example-func ()
  (let ((result (cltpt/combinator:parse
                 "#+begin_example: test"
                 (list cltpt/org-mode::*org-keyword-rule*))))
    result))

(test org-block-example
  (fiveam:is
   (compare-full-match-loosely
    (car (org-block-example-func))
    '((:BEGIN 0 :END 21 :ID CLTPT/ORG-MODE::ORG-KEYWORD)
      ((:BEGIN 0 :END 2 :MATCH "#+"))
      ((:BEGIN 2 :END 16 :ID KEYWORD :MATCH "begin_example"))
      ((:BEGIN 16 :END 17 :MATCH ":"))
      ((:BEGIN 17 :END 18 :MATCH " "))
      ((:BEGIN 18 :END 22 :ID VALUE :MATCH "test"))))))

(defun org-block-with-keywords-func ()
  (let ((result (cltpt/combinator:parse
                 "#+name: my-block"
                 (list cltpt/org-mode::*org-keyword-rule*))))
    result))

(test org-block-with-keywords
  (fiveam:is
   (compare-full-match-loosely
    (car (org-block-with-keywords-func))
    '((:BEGIN 0 :END 14 :ID CLTPT/ORG-MODE::ORG-KEYWORD)
      ((:BEGIN 0 :END 2 :MATCH "#+"))
      ((:BEGIN 2 :END 6 :ID KEYWORD :MATCH "name"))
      ((:BEGIN 6 :END 7 :MATCH ":"))
      ((:BEGIN 7 :END 8 :MATCH " "))
      ((:BEGIN 8 :END 15 :ID VALUE :MATCH "my-block"))))))

(defun org-babel-results-simple-func ()
  (let ((result (cltpt/combinator:parse
                 "#+RESULTS:
: 42"
                 (list cltpt/org-mode::*org-babel-results-rule*))))
    result))

(test org-babel-results-simple
  (fiveam:is
   (compare-full-match-loosely
    (car (org-babel-results-simple-func))
    '((:BEGIN 0 :END 12 :ID CLTPT/ORG-MODE::ORG-BABEL-RESULTS)
      ((:BEGIN 0 :END 10 :MATCH "#+RESULTS:
"))
      ((:BEGIN 10 :END 12 :ID CLTPT/ORG-MODE::ORG-TEXT)
       ((:BEGIN 10 :END 12 :MATCH ": 42")))))))

(defun org-babel-results-with-hash-func ()
  (let ((result (cltpt/combinator:parse
                 "#+RESULTS[abc123def]:
: output here"
                 (list cltpt/org-mode::*org-babel-results-rule*))))
    result))

(test org-babel-results-with-hash
  (fiveam:is
   (compare-full-match-loosely
    (car (org-babel-results-with-hash-func))
    '((:BEGIN 0 :END 28 :ID CLTPT/ORG-MODE::ORG-BABEL-RESULTS)
      ((:BEGIN 0 :END 22 :MATCH "#+RESULTS[abc123def]:
"))
      ((:BEGIN 22 :END 28 :ID CLTPT/ORG-MODE::ORG-TEXT)
       ((:BEGIN 22 :END 28 :MATCH ": output here")))))))

(defun org-babel-results-comprehensive-func ()
  (let ((result (cltpt/combinator:scan-all-rules
                 nil
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
                 (org-rules))))
    result))

(test org-babel-results-comprehensive
  (fiveam:is
   (compare-full-match-loosely
    (car (org-babel-results-comprehensive-func))
    '((:ID CLTPT/ORG-MODE::ORG-BABEL-RESULTS)))))

(defun org-latex-env-basic-func ()
  (let ((result (cltpt/combinator:parse
                 "#+latex: \\begin{equation}"
                 (list cltpt/org-mode::*org-keyword-rule*))))
    result))

(test org-latex-env-basic
  (fiveam:is
   (compare-full-match-loosely
    (car (org-latex-env-basic-func))
    '((:BEGIN 0 :END 26 :ID CLTPT/ORG-MODE::ORG-KEYWORD)
      ((:BEGIN 0 :END 2 :MATCH "#+"))
      ((:BEGIN 2 :END 7 :ID KEYWORD :MATCH "latex"))
      ((:BEGIN 7 :END 8 :MATCH ":"))
      ((:BEGIN 8 :END 9 :MATCH " "))
      ((:BEGIN 9 :END 26 :ID VALUE :MATCH "\\begin{equation}"))))))

(defun org-latex-env-with-name-func ()
  (let ((result (cltpt/combinator:parse
                 "#+name: eq1"
                 (list cltpt/org-mode::*org-keyword-rule*))))
    result))

(test org-latex-env-with-name
  (fiveam:is
   (compare-full-match-loosely
    (car (org-latex-env-with-name-func))
    '((:BEGIN 0 :END 12 :ID CLTPT/ORG-MODE::ORG-KEYWORD)
      ((:BEGIN 0 :END 2 :MATCH "#+"))
      ((:BEGIN 2 :END 6 :ID KEYWORD :MATCH "name"))
      ((:BEGIN 6 :END 7 :MATCH ":"))
      ((:BEGIN 7 :END 8 :MATCH " "))
      ((:BEGIN 8 :END 11 :ID VALUE :MATCH "eq1"))))))

(defun latex-env-parse-test-1 ()
  (cltpt/combinator::parse
   "
\\begin{gather}
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
          `((:pattern ,(cltpt/combinator:handle-rule-string "%w world")
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
    (fiveam:is
     (compare-full-match-loosely
      result
      '((:ID CLTPT/ORG-MODE:ORG-LIST :BEGIN 0 :END 469 :INDENT 0)
        ((:ID CLTPT/ORG-MODE::LIST-ITEM :INDENT 0 :BEGIN 0 :END 148)
         ((:ID CLTPT/ORG-MODE::LIST-ITEM-BULLET :BEGIN 0 :END 1))
         ((:ID CLTPT/ORG-MODE::LIST-ITEM-CONTENT :BEGIN 2 :END 148)))
        ((:ID CLTPT/ORG-MODE::LIST-ITEM :INDENT 0 :BEGIN 148 :END 469)
         ((:ID CLTPT/ORG-MODE::LIST-ITEM-BULLET :BEGIN 148 :END 149))
         ((:ID CLTPT/ORG-MODE::LIST-ITEM-CONTENT :BEGIN 150 :END 469))))))))

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

(defun test-parse-any-func-2 ()
  (cltpt/combinator:parse
   "[[hello:hey][wow]]"
   (list
    `(:pattern (cltpt/combinator::any
                (:pattern (cltpt/combinator:literal "[")
                 :id my-literal))
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
  (let* ((other-rules `((:pattern ,(cltpt/combinator:handle-rule-string "#+%w")
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
  (let* ((other-rules `((:pattern ,(cltpt/combinator:handle-rule-string "#+%w")
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
              ,(cltpt/combinator::handle-rule-string "#%W")
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

(defun test-eol-func ()
  (let* ((rule1 `(:pattern
                  (cltpt/combinator::followed-by
                   (:pattern ,(cltpt/combinator::handle-rule-string "#%W")
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
    (fiveam:is (compare-full-match-loosely
                (car result)
                '((:BEGIN 0 :END 5)
                  ((:BEGIN 0 :END 1))
                  ((:BEGIN 1 :END 5)))))
    (fiveam:is (compare-full-match-loosely
                (cadr result)
                '((:BEGIN 40 :END 45)
                  ((:BEGIN 40 :END 41))
                  ((:BEGIN 41 :END 45)))))))

(defun test-bol-func ()
  (let* ((rule1 `(:pattern
                  (cltpt/combinator::when-match
                   (:pattern ,(cltpt/combinator::handle-rule-string "#%W")
                    :id hashtag)
                   cltpt/combinator:at-line-start-p)
                  :id bol-tag)))
    (cltpt/combinator::scan-all-rules
     nil
     "#tag1 is on line 1
 this is not a match: #tag2
#tag3 is on line 3"
     (list rule1))))

(test test-bol
  (let ((result (test-bol-func)))
    (fiveam:is (= (length result) 2))
    (fiveam:is (compare-full-match-loosely
                (car result)
                '((:BEGIN 0 :END 5)
                  ((:BEGIN 0 :END 1))
                  ((:BEGIN 1 :END 5)))))
    (fiveam:is (compare-full-match-loosely
                (cadr result)
                '((:BEGIN 47 :END 52)
                  ((:BEGIN 47 :END 48))
                  ((:BEGIN 48 :END 52)))))))

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

(test test-org-latex-env-1
  (let ((result (test-org-latex-env-1)))
    (fiveam:is (typep result 'cltpt/base:document))
    (let ((children (cltpt/base:text-object-children result)))
      (fiveam:is (> (length children) 0))
      (let ((latex-env (first children)))
        (fiveam:is (not (null latex-env))) ; ensure not nil
        (fiveam:is (typep latex-env 'cltpt/org-mode::org-latex-env))
        ;; Check the children of the latex-env
        (let ((latex-children (cltpt/base:text-object-children latex-env)))
          (fiveam:is (>= (length latex-children) 1))
          ;; Find the name keyword and latex content
          (let ((name-keyword (find-if (lambda (child) 
                                         (typep child 'cltpt/org-mode::org-keyword))
                                       latex-children))
                (latex-env-content (find-if (lambda (child) 
                                              (typep child 'cltpt/latex::latex-env))
                                            latex-children)))
            ;; Check the name keyword
            (when name-keyword
              (fiveam:is (not (null (search "test-name" (cltpt/base:text-object-text name-keyword))))))
            ;; Check the latex environment content
            (when latex-env-content
              (fiveam:is (not (null (search "equation" (cltpt/base:text-object-text latex-env-content))))))))))))

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

(defun test-org-document-parse-func ()
  "Test parsing a complete org document with multiple elements."
  (cltpt/base:parse
   cltpt/org-mode:*org-mode*
   "#+title: Test Document
#+author: John Doe

* Introduction
This is a test document with various org-mode elements.

** Math Example
#+name: eq1
\\begin{equation}
E = mc^2
\\end{equation}

** Code Example
#+begin_src python
def hello():
    print(\"Hello World\")
#+end_src

** Image Example
#+begin_src python :results file :exports both
import matplotlib.pyplot as plt
import numpy as np

x = np.linspace(0, 10, 100)
y = np.sin(x)
plt.plot(x, y)
plt.savefig('plot.png')
plt.close()
#+end_src

#+RESULTS:
[[file:plot.png]]

** List Example
- Item 1
- Item 2
  - Nested item
- Item 3

| Name | Age |
|------+-----|
| John | 25  |
| Jane | 30  |"))

(test test-org-document-parse
  (let ((result (test-org-document-parse-func)))
    (fiveam:is (typep result 'cltpt/base:document))
    (let ((children (cltpt/base:text-object-children result)))
      ;; check that we have exactly 3 top-level elements
      (fiveam:is (= (length children) 3))

      ;; check title keyword (first child)
      (let ((title-keyword (first children)))
        (fiveam:is (typep title-keyword 'cltpt/org-mode::org-keyword))
        (fiveam:is (not (null (search "title" (cltpt/base:text-object-text title-keyword)))))
        (fiveam:is (not (null (search "Test Document" (cltpt/base:text-object-text title-keyword))))))

      ;; check author keyword (second child)
      (let ((author-keyword (second children)))
        (fiveam:is (typep author-keyword 'cltpt/org-mode::org-keyword))
        (fiveam:is (not (null (search "author" (cltpt/base:text-object-text author-keyword)))))
        (fiveam:is (not (null (search "John Doe" (cltpt/base:text-object-text author-keyword))))))

      ;; check main introduction header (third child) with nested sub-headers
      (let ((intro-header (third children)))
        (fiveam:is (typep intro-header 'cltpt/org-mode::org-header))
        (fiveam:is (not (null (search "Introduction" (cltpt/base:text-object-text intro-header)))))

        ;; check that the intro header has 4 sub-headers
        (let ((sub-headers (cltpt/base:text-object-children intro-header)))
          (fiveam:is (= (length sub-headers) 4))

          ;; check math example sub-header
          (let ((math-header (first sub-headers)))
            (fiveam:is (typep math-header 'cltpt/org-mode::org-header))
            (fiveam:is (not (null (search "Math Example" (cltpt/base:text-object-text math-header)))))
            ;; check that this header has a latex-env child
            (let ((math-children (cltpt/base:text-object-children math-header)))
              (fiveam:is (= (length math-children) 1))
              (let ((latex-env (first math-children)))
                (fiveam:is (typep latex-env 'cltpt/org-mode::org-latex-env))
                (fiveam:is (not (null (search "eq1" (cltpt/base:text-object-text latex-env))))))))

          ;; check code example sub-header
          (let ((code-header (second sub-headers)))
            (fiveam:is (typep code-header 'cltpt/org-mode::org-header))
            (fiveam:is (not (null (search "Code Example" (cltpt/base:text-object-text code-header)))))
            ;; check that this header has a src-block child
            (let ((code-children (cltpt/base:text-object-children code-header)))
              (fiveam:is (= (length code-children) 1))
              (let ((src-block (first code-children)))
                (fiveam:is (typep src-block 'cltpt/org-mode::org-src-block))
                (fiveam:is (not (null (search "python" (cltpt/base:text-object-text src-block))))))))

          ;; check image example sub-header
          (let ((image-header (third sub-headers)))
            (fiveam:is (typep image-header 'cltpt/org-mode::org-header))
            (fiveam:is (not (null (search "Image Example" (cltpt/base:text-object-text image-header)))))
            ;; check that this header has children (src-block and possibly RESULTS)
            (let ((image-children (cltpt/base:text-object-children image-header)))
              (fiveam:is (> (length image-children) 0))
              ;; Find the src-block among children
              (let ((src-block (find-if (lambda (child) 
                                          (typep child 'cltpt/org-mode::org-src-block))
                                        image-children)))
                (fiveam:is (not (null src-block)))
                (when src-block
                  (fiveam:is (not (null (search "matplotlib" (cltpt/base:text-object-text src-block)))))
                  ;; Check if RESULTS content is either in src-block or separate
                  (let ((has-results (or (not (null (search "RESULTS" (cltpt/base:text-object-text src-block))))
                                         (find-if (lambda (child) 
                                                    (and (not (typep child 'cltpt/org-mode::org-src-block))
                                                         (search "RESULTS" (cltpt/base:text-object-text child))))
                                                  image-children))))
                    (fiveam:is (not (null has-results)))
                    ;; Check for plot.png in either src-block or any child
                    (let ((has-plot (or (not (null (search "plot.png" (cltpt/base:text-object-text src-block))))
                                        (find-if (lambda (child) 
                                                   (search "plot.png" (cltpt/base:text-object-text child)))
                                                 image-children))))
                      (fiveam:is (not (null has-plot)))))))))

          ;; find and check list example header with list and table
          (let ((list-header
                  (find-if
                   (lambda (child)
                     (and (typep child 'cltpt/org-mode::org-header)
                          (search "List Example" (cltpt/base:text-object-text child))))
                   sub-headers)))
            (fiveam:is (not (null list-header)))
            ;; check that this header has list and table children
            (let ((header-children (cltpt/base:text-object-children list-header)))
              (fiveam:is (> (length header-children) 0))
              (let ((list-obj
                      (find-if
                       (lambda (child)
                         (typep child 'cltpt/org-mode::org-list))
                       header-children))
                    (table-obj
                      (find-if
                       (lambda (child)
                         (typep child 'cltpt/org-mode::org-table))
                       header-children)))
                ;; Check list
                (when list-obj
                  (fiveam:is (typep list-obj 'cltpt/org-mode::org-list)))
                ;; Check table
                (when table-obj
                  (fiveam:is (typep table-obj 'cltpt/org-mode::org-table))
                  (fiveam:is (not (null (search "Name" (cltpt/base:text-object-text table-obj))))))))))))))

(defun test-org-src-block-with-image-result-func ()
  "Test parsing a src block that generates an image result."
  (cltpt/base:parse
   cltpt/org-mode:*org-mode*
   "#+begin_src python :results file :exports both
import matplotlib.pyplot as plt
import numpy as np

x = np.linspace(0, 10, 100)
y = np.sin(x)
plt.plot(x, y)
plt.savefig('plot.png')
plt.close()
#+end_src

#+RESULTS:
[[file:plot.png]]"))

(test test-org-src-block-with-image-result
  (let ((result (test-org-src-block-with-image-result-func)))
    (fiveam:is (typep result 'cltpt/base:document))
    (let ((children (cltpt/base:text-object-children result)))
      ;; Should have 1 child: src-block (RESULTS is embedded)
      (fiveam:is (= (length children) 1))
      
      ;; Check src-block
      (let ((src-block (first children)))
        (fiveam:is (typep src-block 'cltpt/org-mode::org-src-block))
        (fiveam:is (not (null (search "python" (cltpt/base:text-object-text src-block)))))
        (fiveam:is (not (null (search "matplotlib" (cltpt/base:text-object-text src-block)))))
        ;; Check that RESULTS content is embedded in src-block
        (fiveam:is (not (null (search "RESULTS" (cltpt/base:text-object-text src-block)))))
        (fiveam:is (not (null (search "plot.png" (cltpt/base:text-object-text src-block)))))))))

(defun test-comprehensive-org-document-func ()
  "test parsing a comprehensive org document with many features."
  (cltpt/base:parse
   cltpt/org-mode:*org-mode*
   "#+title: Comprehensive Org Document
#+author: Jane Smith
#+date: 2024-01-15
#+email: jane@example.com
#+options: toc:t num:t
#+latex_class: article
#+startup: overview

* Introduction
This is a comprehensive test document covering many org-mode features.

** Text Formatting
Here we test *bold*, /italic/, _underline_, =verbatim=, ~code~, +strikethrough+ text.

** Links and URLs
- Internal link: [[#target-section][Target Section]]
- External URL: https://www.example.com
- File link: [[file:document.pdf][PDF Document]]
- Image link: [[file:image.png][Description]]
- Email link: [[mailto:user@example.com][Send Email]]

** Lists
*** Unordered List
- First item
- Second item
  - Nested item 1
  - Nested item 2
    - Deeply nested item
- Third item

*** Ordered List  
1. First step
2. Second step
   1. Sub-step 2.1
   2. Sub-step 2.2
3. Third step

*** Description List
- Term 1 :: Description of term 1
- Term 2 :: Description of term 2 with [[link][reference]]

** Source Code Blocks
*** Python Example
#+begin_src python :results output :exports both
def fibonacci(n):
    if n <= 1:
        return n
    return fibonacci(n-1) + fibonacci(n-2)

print(fibonacci(10))
#+end_src

#+RESULTS:
: 55

*** JavaScript Example
#+begin_src javascript :tangle script.js
function greet(name) {
    console.log(\`Hello, \${name}!\`);
}

greet('World');
#+end_src

*** LaTeX Example
#+begin_src latex :exports results
\\begin{equation}
\\int_{0}^{\\infty} e^{-x^2} dx = \\frac{\\sqrt{\\pi}}{2}
\\end{equation}
#+end_src

** Mathematical Content
*** Inline Math
The equation $E = mc^2$ is famous, and so is $\\sum_{i=1}^{n} i = \\frac{n(n+1)}{2}$.

*** LaTeX Environments
#+name: integral
\\begin{equation}
\\int_{a}^{b} f(x) dx = F(b) - F(a)
\\end{equation}

#+name: matrix
\\begin{bmatrix}
1 & 2 & 3 \\\\
4 & 5 & 6 \\\\
7 & 8 & 9
\\end{bmatrix}

** Tables
*** Simple Table
| Name | Age | City |
|------+-----+------|
| John | 25  | NYC  |
| Jane | 30  | LA   |
| Bob  | 35  | Chicago |

*** Complex Table with Formula
| Item | Price | Quantity | Total |
|------+-------+----------+-------|
| Book | $20   |        2 | $40   |
| Pen  | $1.5  |       10 | $15   |
| #TBLFM: $4 = $2 * $3 |

** Timestamps and Scheduling
*** Deadlines and Scheduling
- Deadline for project: <2024-02-01 Thu>
- Meeting scheduled: <2024-01-20 Fri 14:00-15:00>
- Repeating task: <2024-01-15 Mon ++1w>
- Time range: <2024-01-01 Mon>--<2024-01-31 Wed>

*** Timestamp Brackets
- [2024-01-15 Mon] (inactive timestamp)
- [2024-01-16 Tue 10:00] (inactive with time)

** Tags and Properties
*** Task with Tags
*** TODO Write comprehensive documentation :documentation:urgent:
:PROPERTIES:
:Effort:   2h
:Assigned: Jane Smith
:Due:      2024-02-01
:END:

*** DONE Review code changes :code:review:
:PROPERTIES:
:Effort:   1h
:Assigned: John Doe
:END:
- State \"DONE\" from \"TODO\" [2024-01-15 Mon 10:30]

** Blocks and Export
*** Quote Block
#+begin_quote
This is a blockquote that spans multiple lines
and demonstrates how org-mode handles quoted text.
#+end_quote

*** Verse Block
#+begin_verse
  Roses are red,
  Violets are blue,
  Sugar is sweet,
  And so are you.
#+end_verse

*** Center Block
#+begin_center
This text is centered
#+end_center

*** Export Blocks
#+begin_export html
<div class=\"custom\">
  <p>This is custom HTML</p>
</div>
#+end_export

#+begin_export latex
\\customsection{Custom LaTeX Content}
This will be processed by LaTeX only.
#+end_export

** Comments and Footnotes
*** Comments
# This is a line comment
This is regular text with a comment inline

*** Footnotes
This text has a footnote[fn:1].

[fn:1] This is the footnote content.

** Advanced Features
*** Macros
#+macro: greeting Hello, $1!

{{{greeting(World)}}}

*** Include Files
#+include: \"other-file.org\" src lisp

*** Bibliography
[[cite:author2024]]

** Target Section :target:
This is the target section referenced from the link above.

** Final Section
The document ends here with a final *bold* statement."))

;; this test was generated by an llm
(test test-comprehensive-org-document-parse
  (let ((result (test-comprehensive-org-document-func)))
    (fiveam:is (typep result 'cltpt/base:document))
    (let ((children (cltpt/base:text-object-children result)))
      ;; Should have 7 top-level keywords + 1 main header = 8 children
      (fiveam:is (= (length children) 8))

      ;; Check keywords (first 7 children)
      (let ((title-keyword (first children))
            (author-keyword (second children))
            (date-keyword (third children))
            (email-keyword (fourth children))
            (options-keyword (fifth children))
            (latex-class-keyword (sixth children))
            (startup-keyword (seventh children)))

        ;; Verify keyword types and content
        (fiveam:is (typep title-keyword 'cltpt/org-mode::org-keyword))
        (fiveam:is (search "title" (cltpt/base:text-object-text title-keyword)))
        (fiveam:is (search "Comprehensive Org Document" (cltpt/base:text-object-text title-keyword)))

        (fiveam:is (typep author-keyword 'cltpt/org-mode::org-keyword))
        (fiveam:is (search "author" (cltpt/base:text-object-text author-keyword)))
        (fiveam:is (search "Jane Smith" (cltpt/base:text-object-text author-keyword)))

        (fiveam:is (typep date-keyword 'cltpt/org-mode::org-keyword))
        (fiveam:is (search "date" (cltpt/base:text-object-text date-keyword)))

        (fiveam:is (typep email-keyword 'cltpt/org-mode::org-keyword))
        (fiveam:is (search "email" (cltpt/base:text-object-text email-keyword)))

        (fiveam:is (typep options-keyword 'cltpt/org-mode::org-keyword))
        (fiveam:is (search "options" (cltpt/base:text-object-text options-keyword)))

        (fiveam:is (typep latex-class-keyword 'cltpt/org-mode::org-keyword))
        (fiveam:is (search "latex_class" (cltpt/base:text-object-text latex-class-keyword)))

        (fiveam:is (typep startup-keyword 'cltpt/org-mode::org-keyword))
        (fiveam:is (search "startup" (cltpt/base:text-object-text startup-keyword))))

      ;; Check main introduction header (8th child)
      (let ((intro-header (eighth children)))
        (fiveam:is (typep intro-header 'cltpt/org-mode::org-header))
        (fiveam:is (search "Introduction" (cltpt/base:text-object-text intro-header)))

        (let ((sub-headers (cltpt/base:text-object-children intro-header)))
          ;; Should have 13 sub-headers under Introduction
          (fiveam:is (= (length sub-headers) 13))

          ;; Check Text Formatting header (1st sub-header)
          (let ((text-format-header (first sub-headers)))
            (fiveam:is (typep text-format-header 'cltpt/org-mode::org-header))
            (fiveam:is (search "Text Formatting" (cltpt/base:text-object-text text-format-header)))
            (let ((text-children (cltpt/base:text-object-children text-format-header)))
              (fiveam:is (> (length text-children) 0))
              ;; Should have org-emph, org-italic, org-inline-code
              (fiveam:is (find-if (lambda (child) (typep child 'cltpt/org-mode::org-emph)) text-children))
              (fiveam:is (find-if (lambda (child) (typep child 'cltpt/org-mode::org-italic)) text-children))
              (fiveam:is (find-if (lambda (child) (typep child 'cltpt/org-mode::org-inline-code)) text-children))))

          ;; Check Links and URLs header (2nd sub-header)
          (let ((links-header (second sub-headers)))
            (fiveam:is (typep links-header 'cltpt/org-mode::org-header))
            (fiveam:is (search "Links and URLs" (cltpt/base:text-object-text links-header)))
            (let ((links-children (cltpt/base:text-object-children links-header)))
              (fiveam:is (= (length links-children) 1))
              (let ((org-list (first links-children)))
                (fiveam:is (typep org-list 'cltpt/org-mode::org-list))
                (let ((list-children (cltpt/base:text-object-children org-list)))
                  ;; Should have web-link and multiple org-link elements
                  (fiveam:is (find-if (lambda (child) (typep child 'cltpt/org-mode::web-link)) list-children))
                  (fiveam:is (> (count-if (lambda (child) (typep child 'cltpt/org-mode::org-link)) list-children) 0))))))

          ;; Check Lists header (3rd sub-header) with nested sub-headers
          (let ((lists-header (third sub-headers)))
            (fiveam:is (typep lists-header 'cltpt/org-mode::org-header))
            (fiveam:is (search "Lists" (cltpt/base:text-object-text lists-header)))
            (let ((list-sub-headers (cltpt/base:text-object-children lists-header)))
              (fiveam:is (= (length list-sub-headers) 3))
              ;; Check Unordered List, Ordered List, Description List
              (fiveam:is (find-if (lambda (child)
                                    (and (typep child 'cltpt/org-mode::org-header)
                                         (search "Unordered List" (cltpt/base:text-object-text child))))
                                  list-sub-headers))
              (fiveam:is (find-if (lambda (child)
                                    (and (typep child 'cltpt/org-mode::org-header)
                                         (search "Ordered List" (cltpt/base:text-object-text child))))
                                  list-sub-headers))
              (fiveam:is (find-if (lambda (child)
                                    (and (typep child 'cltpt/org-mode::org-header)
                                         (search "Description List" (cltpt/base:text-object-text child))))
                                  list-sub-headers))))

          ;; Check Source Code Blocks header (4th sub-header)
          (let ((source-header (fourth sub-headers)))
            (fiveam:is (typep source-header 'cltpt/org-mode::org-header))
            (fiveam:is (search "Source Code Blocks" (cltpt/base:text-object-text source-header)))
            (let ((source-sub-headers (cltpt/base:text-object-children source-header)))
              (fiveam:is (= (length source-sub-headers) 3))
              ;; Should have Python, JavaScript, LaTeX examples
              (fiveam:is (find-if (lambda (child)
                                    (and (typep child 'cltpt/org-mode::org-header)
                                         (search "Python Example" (cltpt/base:text-object-text child))))
                                  source-sub-headers))
              (fiveam:is (find-if (lambda (child)
                                    (and (typep child 'cltpt/org-mode::org-header)
                                         (search "JavaScript Example" (cltpt/base:text-object-text child))))
                                  source-sub-headers))
              (fiveam:is (find-if (lambda (child)
                                    (and (typep child 'cltpt/org-mode::org-header)
                                         (search "LaTeX Example" (cltpt/base:text-object-text child))))
                                  source-sub-headers))
              ;; Check that each has src-block
              (let ((python-header (find-if (lambda (child)
                                              (search "Python Example" (cltpt/base:text-object-text child)))
                                            source-sub-headers)))
                (when python-header
                  (let ((python-children (cltpt/base:text-object-children python-header)))
                    (fiveam:is (= (length python-children) 1))
                    (fiveam:is (typep (first python-children) 'cltpt/org-mode::org-src-block)))))))

          ;; Check Mathematical Content header (5th sub-header)
          (let ((math-header (fifth sub-headers)))
            (fiveam:is (typep math-header 'cltpt/org-mode::org-header))
            (fiveam:is (search "Mathematical Content" (cltpt/base:text-object-text math-header)))
            (let ((math-sub-headers (cltpt/base:text-object-children math-header)))
              (fiveam:is (= (length math-sub-headers) 2))
              ;; Should have Inline Math and LaTeX Environments
              (fiveam:is (find-if (lambda (child)
                                    (and (typep child 'cltpt/org-mode::org-header)
                                         (search "Inline Math" (cltpt/base:text-object-text child))))
                                  math-sub-headers))
              (fiveam:is (find-if (lambda (child)
                                    (and (typep child 'cltpt/org-mode::org-header)
                                         (search "LaTeX Environments" (cltpt/base:text-object-text child))))
                                  math-sub-headers))
              ;; Check LaTeX environments
              (let ((latex-env-header (find-if (lambda (child)
                                                 (search "LaTeX Environments" (cltpt/base:text-object-text child)))
                                               math-sub-headers)))
                (when latex-env-header
                  (let ((latex-children (cltpt/base:text-object-children latex-env-header)))
                    (fiveam:is (= (length latex-children) 2))
                    ;; Should have org-latex-env with org-keyword children
                    (fiveam:is (every (lambda (child)
                                        (typep child 'cltpt/org-mode::org-latex-env))
                                      latex-children))
                    (let ((first-latex (first latex-children)))
                      (fiveam:is (typep first-latex 'cltpt/org-mode::org-latex-env))
                      (let ((latex-inner-children (cltpt/base:text-object-children first-latex)))
                        (fiveam:is (= (length latex-inner-children) 1))
                        (fiveam:is (typep (first latex-inner-children) 'cltpt/org-mode::org-keyword)))))))))

          ;; Check Tables header (6th sub-header)
          (let ((tables-header (sixth sub-headers)))
            (fiveam:is (typep tables-header 'cltpt/org-mode::org-header))
            (fiveam:is (search "Tables" (cltpt/base:text-object-text tables-header)))
            (let ((tables-sub-headers (cltpt/base:text-object-children tables-header)))
              (fiveam:is (= (length tables-sub-headers) 2))
              ;; Should have Simple Table and Complex Table
              (fiveam:is (find-if (lambda (child)
                                    (and (typep child 'cltpt/org-mode::org-header)
                                         (search "Simple Table" (cltpt/base:text-object-text child))))
                                  tables-sub-headers))
              (fiveam:is (find-if (lambda (child)
                                    (and (typep child 'cltpt/org-mode::org-header)
                                         (search "Complex Table" (cltpt/base:text-object-text child))))
                                  tables-sub-headers))
              ;; Check that each has org-table
              (let ((simple-table (find-if (lambda (child)
                                             (search "Simple Table" (cltpt/base:text-object-text child)))
                                           tables-sub-headers)))
                (when simple-table
                  (let ((table-children (cltpt/base:text-object-children simple-table)))
                    (fiveam:is (= (length table-children) 1))
                    (fiveam:is (typep (first table-children) 'cltpt/org-mode::org-table)))))))

          ;; Check Timestamps and Scheduling header (7th sub-header)
          (let ((timestamps-header (seventh sub-headers)))
            (fiveam:is (typep timestamps-header 'cltpt/org-mode::org-header))
            (fiveam:is (search "Timestamps and Scheduling" (cltpt/base:text-object-text timestamps-header)))
            (let ((timestamp-sub-headers (cltpt/base:text-object-children timestamps-header)))
              (fiveam:is (= (length timestamp-sub-headers) 2))
              ;; Should have Deadlines and Timestamp Brackets
              (fiveam:is (find-if (lambda (child)
                                    (and (typep child 'cltpt/org-mode::org-header)
                                         (search "Deadlines" (cltpt/base:text-object-text child))))
                                  timestamp-sub-headers))
              (fiveam:is (find-if (lambda (child)
                                    (and (typep child 'cltpt/org-mode::org-header)
                                         (search "Timestamp Brackets" (cltpt/base:text-object-text child))))
                                  timestamp-sub-headers))))

          ;; Check Tags and Properties header (8th sub-header)
          (let ((tags-header (eighth sub-headers)))
            (fiveam:is (typep tags-header 'cltpt/org-mode::org-header))
            (fiveam:is (search "Tags and Properties" (cltpt/base:text-object-text tags-header)))
            (let ((tags-sub-headers (cltpt/base:text-object-children tags-header)))
              (fiveam:is (= (length tags-sub-headers) 3))
              ;; Should have Task with Tags, TODO task, DONE task
              (fiveam:is (find-if (lambda (child)
                                    (and (typep child 'cltpt/org-mode::org-header)
                                         (search "TODO" (cltpt/base:text-object-text child))))
                                  tags-sub-headers))
              (fiveam:is (find-if (lambda (child)
                                    (and (typep child 'cltpt/org-mode::org-header)
                                         (search "DONE" (cltpt/base:text-object-text child))))
                                  tags-sub-headers))
              ;; Check property drawer
              (let ((todo-header (find-if (lambda (child)
                                            (search "TODO" (cltpt/base:text-object-text child)))
                                          tags-sub-headers)))
                (when todo-header
                  (let ((todo-children (cltpt/base:text-object-children todo-header)))
                    (fiveam:is (= (length todo-children) 1))
                    (fiveam:is (typep (first todo-children) 'cltpt/org-mode::org-prop-drawer)))))))

          ;; Check Blocks and Export header (9th sub-header)
          (let ((blocks-header (ninth sub-headers)))
            (fiveam:is (typep blocks-header 'cltpt/org-mode::org-header))
            (fiveam:is (search "Blocks and Export" (cltpt/base:text-object-text blocks-header)))
            (let ((blocks-sub-headers (cltpt/base:text-object-children blocks-header)))
              (fiveam:is (= (length blocks-sub-headers) 4))
              ;; Should have Quote, Verse, Center, Export blocks
              (fiveam:is (find-if (lambda (child)
                                    (and (typep child 'cltpt/org-mode::org-header)
                                         (search "Quote Block" (cltpt/base:text-object-text child))))
                                  blocks-sub-headers))
              (fiveam:is (find-if (lambda (child)
                                    (and (typep child 'cltpt/org-mode::org-header)
                                         (search "Verse Block" (cltpt/base:text-object-text child))))
                                  blocks-sub-headers))
              (fiveam:is (find-if (lambda (child)
                                    (and (typep child 'cltpt/org-mode::org-header)
                                         (search "Center Block" (cltpt/base:text-object-text child))))
                                  blocks-sub-headers))
              (fiveam:is (find-if (lambda (child)
                                    (and (typep child 'cltpt/org-mode::org-header)
                                         (search "Export Blocks" (cltpt/base:text-object-text child))))
                                  blocks-sub-headers))
              ;; Check block types
              (let ((quote-header (find-if (lambda (child)
                                             (search "Quote Block" (cltpt/base:text-object-text child)))
                                           blocks-sub-headers)))
                (when quote-header
                  (let ((quote-children (cltpt/base:text-object-children quote-header)))
                    (fiveam:is (= (length quote-children) 1))
                    (fiveam:is (typep (first quote-children) 'cltpt/org-mode::org-block)))))
              (let ((export-header (find-if (lambda (child)
                                              (search "Export Blocks" (cltpt/base:text-object-text child)))
                                            blocks-sub-headers)))
                (when export-header
                  (let ((export-children (cltpt/base:text-object-children export-header)))
                    (fiveam:is (= (length export-children) 2))
                    (fiveam:is (every (lambda (child)
                                        (typep child 'cltpt/org-mode::org-export-block))
                                      export-children)))))))

          ;; Check Comments and Footnotes header (10th sub-header)
          (let ((comments-header (tenth sub-headers)))
            (fiveam:is (typep comments-header 'cltpt/org-mode::org-header))
            (fiveam:is (search "Comments and Footnotes" (cltpt/base:text-object-text comments-header)))
            (let ((comments-sub-headers (cltpt/base:text-object-children comments-header)))
              (fiveam:is (= (length comments-sub-headers) 2))
              ;; Should have Comments and Footnotes
              (fiveam:is (find-if (lambda (child)
                                    (and (typep child 'cltpt/org-mode::org-header)
                                         (search "Comments" (cltpt/base:text-object-text child))))
                                  comments-sub-headers))
              (fiveam:is (find-if (lambda (child)
                                    (and (typep child 'cltpt/org-mode::org-header)
                                         (search "Footnotes" (cltpt/base:text-object-text child))))
                                  comments-sub-headers))
              ;; Check comment
              (let ((comment-header (find-if (lambda (child)
                                               (search "Comments" (cltpt/base:text-object-text child)))
                                             comments-sub-headers)))
                (when comment-header
                  (let ((comment-children (cltpt/base:text-object-children comment-header)))
                    (fiveam:is (= (length comment-children) 1))
                    (fiveam:is (typep (first comment-children) 'cltpt/org-mode::org-comment)))))))

          ;; Check Advanced Features header (11th sub-header)
          (let ((advanced-header (nth 10 sub-headers)))
            (fiveam:is (typep advanced-header 'cltpt/org-mode::org-header))
            (fiveam:is (search "Advanced Features" (cltpt/base:text-object-text advanced-header)))
            (let ((advanced-sub-headers (cltpt/base:text-object-children advanced-header)))
              (fiveam:is (= (length advanced-sub-headers) 3))
              ;; Should have Macros, Include, Bibliography
              (fiveam:is (find-if (lambda (child)
                                    (and (typep child 'cltpt/org-mode::org-header)
                                         (search "Macros" (cltpt/base:text-object-text child))))
                                  advanced-sub-headers))
              (fiveam:is (find-if (lambda (child)
                                    (and (typep child 'cltpt/org-mode::org-header)
                                         (search "Include" (cltpt/base:text-object-text child))))
                                  advanced-sub-headers))
              (fiveam:is (find-if (lambda (child)
                                    (and (typep child 'cltpt/org-mode::org-header)
                                         (search "Bibliography" (cltpt/base:text-object-text child))))
                                  advanced-sub-headers))
              ;; Check macro keyword
              (let ((macro-header (find-if (lambda (child)
                                             (search "Macros" (cltpt/base:text-object-text child)))
                                           advanced-sub-headers)))
                (when macro-header
                  (let ((macro-children (cltpt/base:text-object-children macro-header)))
                    (fiveam:is (= (length macro-children) 1))
                    (fiveam:is (typep (first macro-children) 'cltpt/org-mode::org-keyword))
                    (fiveam:is (search "macro" (cltpt/base:text-object-text (first macro-children)))))))))

          ;; Check Target Section header (12th sub-header)
          (let ((target-header (nth 11 sub-headers)))
            (fiveam:is (typep target-header 'cltpt/org-mode::org-header))
            (fiveam:is (search "Target Section" (cltpt/base:text-object-text target-header))))

          ;; Check Final Section header (13th sub-header)
          (let ((final-header (nth 12 sub-headers)))
            (fiveam:is (typep final-header 'cltpt/org-mode::org-header))
            (fiveam:is (search "Final Section" (cltpt/base:text-object-text final-header)))
            (let ((final-children (cltpt/base:text-object-children final-header)))
              (fiveam:is (= (length final-children) 1))
              (fiveam:is (typep (first final-children) 'cltpt/org-mode::org-emph)))))))))

(defun compare-tree-types (actual-tree expected-types-tree &optional (path "root"))
  "Iterate through two trees simultaneously and check if types match.
ACTUAL-TREE is the parsed text-object tree.
EXPECTED-TYPES-TREE is a cons tree of type symbols.
Uses the cltpt/tree interface for both trees.
Returns a list of error messages if types don't match."
  (let ((errors))
    (labels
      ((safe-tree-children (node)
         "Safely get children, handling cases where tree-children might not be applicable"
         (handler-case
             (cltpt/tree:tree-children node)
           (error () nil)))

       (compare-nodes (actual-node expected-node current-path)
         ;; Check if we have both nodes
         (cond
           ;; Both are nil - end of branch
           ((and (null actual-node) (null expected-node))
            nil)

           ;; One is nil but not the other - structure mismatch
           ((null actual-node)
            (push (format nil "~a: Expected node of type ~a, but got nil"
                         current-path (cltpt/tree:tree-value expected-node)) errors))

           ((null expected-node)
            (push (format nil "~a: Got unexpected node of type ~a"
                         current-path (type-of actual-node)) errors))

           ;; Both exist - compare types and recurse
           (t
            ;; Get values and children using tree interface
            (let ((actual-type (type-of actual-node))
                  (actual-children (safe-tree-children actual-node))
                  (expected-type (cltpt/tree:tree-value expected-node))
                  (expected-children (safe-tree-children expected-node)))

              ;; Compare types - actual-type is a symbol, expected-type comes from tree-value
              (unless (if (symbolp expected-type)
                        (equal actual-type expected-type)  ; Both symbols
                        (string= (symbol-name actual-type) expected-type))  ; Symbol vs string
                (push (format nil "~a: Expected type ~a, got ~a"
                             current-path expected-type actual-type) errors))

              ;; Compare children count
              (unless (= (length actual-children) (length expected-children))
                (push (format nil "~a: Expected ~d children, got ~d"
                             current-path (length expected-children) (length actual-children)) errors))

              ;; Recurse on children - iterate through both trees simultaneously
              (loop for actual-child in actual-children
                    for expected-child in expected-children
                    for i from 0
                    do (compare-nodes actual-child expected-child
                                    (format nil "~a[~d]" current-path i))))))))

      (compare-nodes actual-tree expected-types-tree path))
    (nreverse errors)))

(defun create-expected-types-tree ()
  "Create a cons tree of expected types for the comprehensive org document.
This exactly matches the actual parsed tree structure."
  (cons 'cltpt/org-mode::org-document
        (append
         ;; 7 keywords at the top level (as cons cells with no children)
         (list (cons 'cltpt/org-mode::org-keyword nil)
               (cons 'cltpt/org-mode::org-keyword nil)
               (cons 'cltpt/org-mode::org-keyword nil)
               (cons 'cltpt/org-mode::org-keyword nil)
               (cons 'cltpt/org-mode::org-keyword nil)
               (cons 'cltpt/org-mode::org-keyword nil)
               (cons 'cltpt/org-mode::org-keyword nil))
         ;; Main header that contains all sub-headers (13 of them)
         (list
          (cons 'cltpt/org-mode::org-header
                (list
                 ;; 1. Text Formatting section
                 (cons 'cltpt/org-mode::org-header
                       (list 'cltpt/org-mode::org-emph
                             'cltpt/org-mode::org-italic
                             'cltpt/org-mode::org-inline-code))
                 ;; 2. Links and Images section
                 (cons 'cltpt/org-mode::org-header
                       (list (cons 'cltpt/org-mode::org-list
                                   (list 'cltpt/org-mode::web-link
                                         'cltpt/org-mode::org-link
                                         'cltpt/org-mode::org-link
                                         'cltpt/org-mode::org-link))))
                 ;; 3. Lists section (3 sub-headers)
                 (cons 'cltpt/org-mode::org-header
                       (list
                        ;; Unordered Lists
                        (cons 'cltpt/org-mode::org-header
                              (list (cons 'cltpt/org-mode::org-list
                                          (list (cons 'cltpt/org-mode::org-list
                                                      (list (cons 'cltpt/org-mode::org-list '())))))))
                        ;; Ordered Lists
                        (cons 'cltpt/org-mode::org-header
                              (list (cons 'cltpt/org-mode::org-list
                                          (list (cons 'cltpt/org-mode::org-list '())))))
                        ;; Description Lists
                        (cons 'cltpt/org-mode::org-header
                              (list (cons 'cltpt/org-mode::org-list '())))))
                 ;; 4. Source Code section (3 sub-headers)
                 (cons 'cltpt/org-mode::org-header
                       (list
                        ;; Python
                        (cons 'cltpt/org-mode::org-header
                              (list 'cltpt/org-mode::org-src-block))
                        ;; JavaScript
                        (cons 'cltpt/org-mode::org-header
                              (list 'cltpt/org-mode::org-src-block))
                        ;; LaTeX
                        (cons 'cltpt/org-mode::org-header
                              (list 'cltpt/org-mode::org-src-block))))
                 ;; 5. Mathematics section (2 sub-headers)
                 (cons 'cltpt/org-mode::org-header
                       (list
                        ;; Inline Math
                        (cons 'cltpt/org-mode::org-header '())
                        ;; LaTeX Environments
                        (cons 'cltpt/org-mode::org-header
                              (list
                               ;; First LaTeX environment with keyword
                               (cons 'cltpt/org-mode::org-latex-env
                                     (list 'cltpt/org-mode::org-keyword))
                               ;; Second LaTeX environment with keyword
                               (cons 'cltpt/org-mode::org-latex-env
                                     (list 'cltpt/org-mode::org-keyword))))))
                 ;; 6. Tables section (2 sub-headers)
                 (cons 'cltpt/org-mode::org-header
                       (list
                        ;; Simple Table
                        (cons 'cltpt/org-mode::org-header
                              (list 'cltpt/org-mode::org-table))
                        ;; Complex Table
                        (cons 'cltpt/org-mode::org-header
                              (list 'cltpt/org-mode::org-table))))
                 ;; 7. Timestamps section (2 sub-headers)
                 (cons 'cltpt/org-mode::org-header
                       (list
                        ;; Deadline
                        (cons 'cltpt/org-mode::org-header
                              (list 'cltpt/org-mode::org-list))
                        ;; Timestamps
                        (cons 'cltpt/org-mode::org-header
                              (list 'cltpt/org-mode::org-list))))
                 ;; 8. Tags and Tasks section (3 sub-headers)
                 (cons 'cltpt/org-mode::org-header
                       (list
                        ;; Task with tags
                        (cons 'cltpt/org-mode::org-header '())
                        ;; TODO with properties drawer
                        (cons 'cltpt/org-mode::org-header
                              (list 'cltpt/org-mode::org-prop-drawer))
                        ;; DONE with properties drawer and list
                        (cons 'cltpt/org-mode::org-header
                              (list 'cltpt/org-mode::org-prop-drawer
                                    'cltpt/org-mode::org-list))))
                 ;; 9. Blocks and Quotes section (4 sub-headers)
                 (cons 'cltpt/org-mode::org-header
                       (list
                        ;; Quote Block
                        (cons 'cltpt/org-mode::org-header
                              (list 'cltpt/org-mode::org-block))
                        ;; Verse Block
                        (cons 'cltpt/org-mode::org-header
                              (list 'cltpt/org-mode::org-block))
                        ;; Center Block
                        (cons 'cltpt/org-mode::org-header
                              (list 'cltpt/org-mode::org-block))
                        ;; Export Blocks (2 of them)
                        (cons 'cltpt/org-mode::org-header
                              (list 'cltpt/org-mode::org-export-block
                                    'cltpt/org-mode::org-export-block))))
                 ;; 10. Comments section (2 sub-headers)
                 (cons 'cltpt/org-mode::org-header
                       (list
                        ;; Comments
                        (cons 'cltpt/org-mode::org-header
                              (list 'cltpt/org-mode::org-comment))
                        ;; Footnotes
                        (cons 'cltpt/org-mode::org-header '())))
                 ;; 11. Advanced Features section (3 sub-headers)
                 (cons 'cltpt/org-mode::org-header
                       (list
                        ;; Macros
                        (cons 'cltpt/org-mode::org-header
                              (list 'cltpt/org-mode::org-keyword))
                        ;; Include
                        (cons 'cltpt/org-mode::org-header
                              (list 'cltpt/org-mode::org-keyword))
                        ;; Bibliography
                        (cons 'cltpt/org-mode::org-header
                              (list 'cltpt/org-mode::org-link))))
                 ;; 12. Target Search section
                 (cons 'cltpt/org-mode::org-header '())
                 ;; 13. Final Section
                 (cons 'cltpt/org-mode::org-header
                       (list 'cltpt/org-mode::org-emph))))))))

(defun test-comprehensive-org-document-parse ()
  "test parsing of comprehensive org document and validate structure"
  (let* ((doc (test-comprehensive-org-document-func))
         (expected-tree (create-expected-types-tree))
         (errors (compare-tree-types doc expected-tree)))
    (if errors
        (progn
          (format t "~&tree type validation failed:~%")
          (loop for error in errors do (format t "  ~a~%" error))
          nil)
        t)))

(test comprehensive-org-document-structure-validation
  (let ((doc (test-comprehensive-org-document-func)))
    (format t "~&actual tree structure (first 3 levels):~%")
    (cltpt/tree:tree-show doc)
    (fiveam:is (test-comprehensive-org-document-parse))))

(defun run-org-mode-tests ()
  "Run all org-mode rules tests."
  (format t "~&running org-mode tests...~%")
  (let ((results (run! 'org-mode-suite)))
    (unless results
      (explain! results))))