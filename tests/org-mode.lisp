(defpackage :cltpt/tests/org-mode
  (:use :cl :it.bese.fiveam)
  (:import-from :cltpt/tests
                :simplify-match
                :compare-match-loosely
                :compare-full-match-loosely
                :string=+diff)
  (:import-from :cltpt/tests/utils
                :org-rules
                :rules-from-symbols))

(in-package :cltpt/tests/org-mode)

(def-suite org-mode-suite
  :description "tests for org-mode."
  :in cltpt/tests::cltpt-suite)

(in-suite org-mode-suite)

(defun make-dummy-context ()
  (let ((rules (org-rules)))
    (cltpt/combinator::make-context-from-rules rules)))

(defun org-table-parse (table-text)
  "parse an org-mode table and return a list of rows, each row being a list of cell values."
  (let* ((reader (cltpt/reader:reader-from-string table-text))
         (parsed (cltpt/org-mode::org-table-matcher nil reader 0)))
    (when parsed
      (loop for row-node in (cdr parsed)
            when (eq (getf (car row-node) :ID) 'CLTPT/ORG-MODE::TABLE-ROW)
              collect (loop for cell-node in (cdr row-node)
                            collect (cltpt/combinator:match-text
                                     reader
                                     (car cell-node)))))))

(defun simplify-full-match (match)
  (labels ((my-simplify (m)
             (simplify-match (car m))))
    (cltpt/tree:tree-map match #'my-simplify)))

(defun org-keyword-basic-func ()
  (let ((result (cltpt/combinator:parse
                 "#+title: My Document"
                 (list cltpt/org-mode::*org-keyword-rule*))))
    result))

(test org-keyword-basic
  (fiveam:is
   (compare-full-match-loosely
    (car (org-keyword-basic-func))
    '((:BEGIN 0 :END 20 :ID CLTPT/ORG-MODE::ORG-KEYWORD)
      ((:BEGIN 0 :END 2 :MATCH "#+"))
      ((:BEGIN 2 :END 7 :ID KEYWORD :MATCH "title"))
      ((:BEGIN 7 :END 8 :MATCH ":"))
      ((:BEGIN 8 :END 9 :MATCH " "))
      ((:BEGIN 9 :END 20 :ID VALUE :MATCH "My Document"))))))

(defun org-comment-basic-func ()
  (let ((result (cltpt/combinator:parse
                 "# this is a comment"
                 (list cltpt/org-mode::*org-comment-rule*))))
    result))

(test org-comment-basic
  (fiveam:is
   (compare-full-match-loosely
    (car (org-comment-basic-func))
    '((:BEGIN 0 :END 19 :ID CLTPT/ORG-MODE::ORG-COMMENT)
      ((:BEGIN 0 :END 1 :MATCH "#"))
      ((:BEGIN 1 :END 2 :MATCH " "))
      ((:BEGIN 2 :END 19 :MATCH "this is a comment"))))))

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
    result))

(test org-header-with-todo
  (fiveam:is
   (compare-full-match-loosely
    (car (org-header-with-todo-func))
    '((:BEGIN 0 :END 14 :ID CLTPT/ORG-MODE::ORG-HEADER)
      ((:BEGIN 0 :END 1 :ID STARS :MATCH "*"))
      ((:BEGIN 1 :END 2 :MATCH " "))
      ((:BEGIN 2 :END 6 :ID TODO :MATCH "TODO"))
      ((:BEGIN 6 :END 7 :MATCH " "))
      ((:BEGIN 7 :END 14 :ID TITLE :MATCH "my header"))))))

(defun org-header-with-tags-func ()
  (let ((result (cltpt/combinator:parse
                 "* my header :tag1:tag2:"
                 (list cltpt/org-mode::*org-header-rule*))))
    result))

(test org-header-with-tags
  (fiveam:is
   (compare-full-match-loosely
    (car (org-header-with-tags-func))
    '((:BEGIN 0 :END 21 :ID CLTPT/ORG-MODE::ORG-HEADER)
      ((:BEGIN 0 :END 1 :ID STARS :MATCH "*"))
      ((:BEGIN 1 :END 2 :MATCH " "))
      ((:BEGIN 2 :END 12 :ID TITLE :MATCH "my header "))
      ((:BEGIN 12 :END 21 :ID TAGS :MATCH ":tag1:tag2:"))))))

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
   (cltpt/combinator:apply-rule-normalized nil cltpt/org-mode::*org-timestamp-rule* (cltpt/reader:reader-from-string "<2023-12-28 Thu>") 0)
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
   (cltpt/combinator:apply-rule-normalized nil cltpt/org-mode::*org-timestamp-rule* (cltpt/reader:reader-from-string "<2023-12-28 Thu 18:30:00>") 0)
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
   (cltpt/combinator:apply-rule-normalized nil cltpt/org-mode::*org-timestamp-rule* (cltpt/reader:reader-from-string "<2023-12-28 Thu 18:30:00 +1w>") 0)
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
   (cltpt/combinator:apply-rule-normalized nil cltpt/org-mode::*org-timestamp-rule* (cltpt/reader:reader-from-string "<2023-12-28 Thu 18:30:00 +1w>--<2023-12-28 Thu 19:00:00 +1w>") 0)
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

(defun org-link-simple-func ()
  (let ((result (cltpt/combinator:parse
                 "[[id:abc123]]"
                 (list cltpt/org-mode::*org-link-rule*))))
    result))

(test org-link-simple
  (fiveam:is
   (compare-full-match-loosely
    (car (org-link-simple-func))
    '((:BEGIN 0 :END 13 :MATCH "[[id:abc123]]")
      ((:BEGIN 0 :END 2 :MATCH "[["))
      ((:BEGIN 2 :END 4 :MATCH "id"))
      ((:BEGIN 4 :END 5 :MATCH ":"))
      ((:BEGIN 5 :END 11 :MATCH "abc123"))
      ((:BEGIN 11 :END 13 :MATCH "]]"))))))

(defun org-link-with-description-func ()
  (let ((result (cltpt/combinator:parse
                 "[[file:document.pdf][My Document]]"
                 (list cltpt/org-mode::*org-link-rule*))))
    result))

(test org-link-with-description
  (fiveam:is
   (compare-full-match-loosely
    (car (org-link-with-description-func))
    '((:BEGIN 0 :END 34 :MATCH "[[file:document.pdf][My Document]]")
      ((:BEGIN 0 :END 2 :MATCH "[["))
      ((:BEGIN 2 :END 6 :MATCH "file"))
      ((:BEGIN 6 :END 7 :MATCH ":"))
      ((:BEGIN 7 :END 19 :MATCH "document.pdf"))
      ((:BEGIN 19 :END 21 :MATCH "]["))
      ((:BEGIN 21 :END 32 :MATCH "My Document"))
      ((:BEGIN 32 :END 34 :MATCH "]]"))))))

(defun org-link-no-type-func ()
  (let ((result (cltpt/combinator:parse
                 "[[document.pdf]]"
                 (list cltpt/org-mode::*org-link-rule*))))
    result))

(test org-link-no-type
  (fiveam:is
   (compare-full-match-loosely
    (car (org-link-no-type-func))
    '((:BEGIN 0 :END 16 :MATCH "[[document.pdf]]")
      ((:BEGIN 0 :END 2 :MATCH "[["))
      ((:BEGIN 2 :END 14 :MATCH "document.pdf"))
      ((:BEGIN 14 :END 16 :MATCH "]]"))))))

(defun org-web-link-basic-func ()
  (let ((result (cltpt/combinator:parse
                 "https://example.com/page"
                 (list cltpt/org-mode::*web-link-rule*))))
    result
    ))

(test org-web-link-basic
  (fiveam:is
   (compare-full-match-loosely
    (car (org-web-link-basic-func))
    '((:BEGIN 0 :END 24 :MATCH "https://example.com/page")
      ((:BEGIN 0 :END 8 :MATCH "https://"))
      ((:BEGIN 8 :END 24 :MATCH "example.com/page"))))))

(defun org-inline-code-basic-func ()
  (let ((result (cltpt/combinator:parse
                 "~code here~"
                 (list cltpt/org-mode::*org-inline-code-rule*))))
    result))

(test org-inline-code-basic
  (fiveam:is
   (compare-full-match-loosely
    (car (org-inline-code-basic-func))
    '((:BEGIN 0 :END 11 :MATCH "~code here~")
      ((:BEGIN 0 :END 1 :MATCH "~"))
      ((:BEGIN 1 :END 10 :MATCH "code here"))
      ((:BEGIN 10 :END 11 :MATCH "~"))))))

(defun org-keywords-basic-func ()
  (let ((result (cltpt/combinator:parse
                 " :hello-there :hello2"
                 (list cltpt/org-mode::*keywords-rule*))))
    result))

(test org-keywords-basic
  (fiveam:is
   (compare-full-match-loosely
    (car (org-keywords-basic-func))
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

(defun org-italic-basic-func ()
  (let ((result (cltpt/combinator:parse
                 "/italic text/"
                 (list cltpt/org-mode::*org-italic-rule*))))
    result))

(test org-italic-basic
  (fiveam:is
   (compare-full-match-loosely
    (car (org-italic-basic-func))
    '((:BEGIN 0 :END 13 :MATCH "/italic text/")
      ((:BEGIN 0 :END 1 :MATCH "/"))
      ((:BEGIN 12 :END 13 :MATCH "/"))))))

(defun org-prop-drawer-basic-test-func ()
  (let ((result (cltpt/combinator:parse
                 ":PROPERTIES:
:ID: my-id-123
:CUSTOM_ID: my-custom
:END:"
                 (list cltpt/org-mode::*org-prop-drawer-rule*))))
    result))

(test org-prop-drawer-basic
  (fiveam:is
   (compare-full-match-loosely
    (car (org-prop-drawer-basic-test-func))
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

(defun org-prop-drawer-empty-test-func ()
  (let ((result (cltpt/combinator:parse
                 ":PROPERTIES:
:END:"
                 (list cltpt/org-mode::*org-prop-drawer-rule*))))
    result))

(test org-prop-drawer-empty
  (fiveam:is
   (compare-full-match-loosely
    (car (org-prop-drawer-empty-test-func))
    '((:BEGIN 0 :END 14 :ID CLTPT/ORG-MODE::ORG-PROP-DRAWER)
      ((:BEGIN 0 :END 12 :ID CLTPT/ORG-MODE::DRAWER-OPEN-TAG :MATCH ":PROPERTIES:"))
      ((:BEGIN 12 :END 13 :MATCH "\n"))
      ((:BEGIN 13 :END 14 :ID CLTPT/ORG-MODE::DRAWER-CLOSE-TAG :MATCH ":END:"))))))

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

;; (test org-babel-results-comprehensive
;;   (fiveam:is
;;    (compare-full-match-loosely
;;     (car (org-babel-results-comprehensive-func))
;;     '((:ID CLTPT/ORG-MODE::ORG-BABEL-RESULTS)))))

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
   "#+name: test-name
\\begin{equation}
my equation here
\\end{equation}"))

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
  (cltpt/org-mode::get-repeat-interval "1" "w"))

(defun test-org-duration-parsing-2 ()
  (cltpt/org-mode::get-repeat-interval "2" "d"))

(defun test-org-duration-parsing-3 ()
  (cltpt/org-mode::get-repeat-interval "3" "h"))

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

(defun test-org-src-block-with-image-result-func ()
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
  (compare-tree-types (test-org-src-block-with-image-result-func)
                      '(org-document (org-src-block org-link))))

(defun test-comprehensive-org-document-func ()
  "test parsing a comprehensive org document with many features."
  (let ((content (uiop:read-file-string "tests/data/comprehensive-org-test.org")))
    (cltpt/base:parse cltpt/org-mode:*org-mode* content)))

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

(defun org-timestamp-1 ()
  (let ((result (cltpt/combinator:parse
                 "<2025-10-14 Tue +1d>"
                 ;; "<2025-10-14 10:00 +1d>"
                 (list cltpt/org-mode::*org-timestamp-rule*))))
    (cltpt/org-mode::handle-time-match (car result))))

(defun run-org-list-extensive-test ()
  (let* ((messy (concatenate 'string
                             "-   Item 1   " (string #\newline)
                             "     Line 1 Continuation" (string #\newline)
                             "     Line 2 Continuation" (string #\newline)
                             "- Item 2"))
         (match (cltpt/org-mode:org-list-matcher nil (cltpt/reader:reader-from-string messy) 0))
         (clean (reformat-list match)))
    (format t "~A~%" messy)
    (format t "~A~%" clean)))

(defun check-list-indexing ()
  (let* ((text (concatenate 'string
                            "- chapter 1" (string #\newline)
                            "- chapter 2" (string #\newline)
                            "  - section 2.a" (string #\newline)
                            "  - section 2.b" (string #\newline)
                            "    - subsection 2.b.1" (string #\newline)
                            "    - subsection 2.b.2 <-- we want this one" (string #\newline)
                            "- chapter 3"))
         (match (cltpt/org-mode:org-list-matcher nil (cltpt/reader:reader-from-string text) 0)))
    (let* ((target-string "subsection 2.b.2")
           (pos (search target-string text)))
      (format t "target text: \"~a\"~%" target-string)
      (format t "found at character position: ~a~%" pos)
      (let ((indices (cltpt/org-mode::get-list-item-indices match pos)))
        (format t "identified indices: ~a~%" indices)
        (let ((node (cltpt/org-mode::get-item-at-indices match indices)))
          (when node
              (let ((content (cltpt/org-mode::get-list-item-text node)))
                (format t "content of retrieved node: \"~a\"~%" content))))))))

(defun run-org-mode-tests ()
  (format t "~&running org-mode tests...~%")
  ;; set up relative paths for latex previews so tests work across different systems
  (setf cltpt/latex-previews::*latex-previews-tmp-directory* "cltpt-latex-previews/tmp/")
  (setf cltpt/latex-previews::*latex-previews-cache-directory* "cltpt-latex-previews/cache/")
  ;; ensure directories exist
  (ensure-directories-exist cltpt/latex-previews::*latex-previews-tmp-directory*)
  (ensure-directories-exist cltpt/latex-previews::*latex-previews-cache-directory*)
  (let ((results (run! 'org-mode-suite)))
    (unless results
      (explain! results))))