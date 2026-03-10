(defpackage :cltpt/tests/org-mode
  (:use :cl :it.bese.fiveam)
  (:import-from
   :cltpt/tests
   :match-tree-equal-p
   :string=+diff
   :is-match-tree)
  (:import-from
   :cltpt/tests/utils
   :org-rules
   :compare-tree-types
   :rules-from-symbols))

(in-package :cltpt/tests/org-mode)

(def-suite org-mode-suite
  :description "tests for org-mode."
  :in cltpt/tests::cltpt-suite)

(in-suite org-mode-suite)

(defun org-table-parse (table-text)
  "parse an org-mode table and return a list of rows, each row being a list of cell values."
  (let* ((reader (cltpt/reader:reader-from-string table-text))
         (parsed (cltpt/org-mode::org-table-matcher nil reader 0)))
    (when parsed
      (loop for row-node in (cdr parsed)
            when (eq (getf (car row-node) :ID) 'CLTPT/ORG-MODE::TABLE-ROW)
              collect (loop for cell-node in (cdr row-node)
                            collect (cltpt/combinator:match-text
                                     (car cell-node)
                                     reader))))))

(defun org-keyword-basic-func ()
  (let ((result (cltpt/combinator:parse
                 "#+title: My Document"
                 (list cltpt/org-mode::org-keyword))))
    result))

(test org-keyword-basic
  (is-match-tree
   (car (org-keyword-basic-func))
   '((:BEGIN 0 :END 20)
     ((:BEGIN 0 :END 8)
      ((:BEGIN 0 :END 2))
      ((:BEGIN 2 :END 7 :ID KEYWORD))
      ((:BEGIN 7 :END 8)))
     ((:BEGIN 8 :END 9))
     ((:BEGIN 9 :END 20)
      ((:BEGIN 0 :END 11 :ID CLTPT/ORG-MODE::VALUE))))))

(defun org-comment-basic-func ()
  (let ((result (cltpt/combinator:parse
                 "# this is a comment"
                 (list cltpt/org-mode::org-comment))))
    result))

(test org-comment-basic
  (is-match-tree
   (car (org-comment-basic-func))
   '((:BEGIN 0 :END 19)
     ((:BEGIN 0 :END 2))
     ((:BEGIN 2 :END 19)))))

(defun org-header-basic-func ()
  (let ((result (cltpt/combinator:parse
                 "* my header"
                 (list cltpt/org-mode::org-header))))
    result))

(test org-header-basic
  (let ((result (org-header-basic-func)))
    (fiveam:is
     (match-tree-equal-p
      (car result)
      '((:BEGIN 0 :END 11 :ID CLTPT/ORG-MODE:ORG-HEADER)
        ((:BEGIN 0 :END 11)
         ((:BEGIN 0 :END 1 :ID CLTPT/ORG-MODE::STARS))
         ((:BEGIN 1 :END 2))
         ((:BEGIN 2 :END 11 :ID CLTPT/ORG-MODE::TITLE))))))))

(defun org-header-with-todo-func ()
  (let ((result (cltpt/combinator:parse
                 "* TODO my header"
                 (list cltpt/org-mode::org-header))))
    result))

(test org-header-with-todo
  (is-match-tree
   (car (org-header-with-todo-func))
   '((:BEGIN 0 :END 16 :ID CLTPT/ORG-MODE:ORG-HEADER)
     ((:BEGIN 0 :END 16)
      ((:BEGIN 0 :END 1 :ID CLTPT/ORG-MODE::STARS))
      ((:BEGIN 1 :END 2))
      ((:BEGIN 2 :END 7)
       ((:BEGIN 0 :END 4 :ID CLTPT/ORG-MODE::TODO-KEYWORD))
       ((:BEGIN 4 :END 5)))
      ((:BEGIN 7 :END 16 :ID CLTPT/ORG-MODE::TITLE))))))

(defun org-header-with-tags-func ()
  (let ((result (cltpt/combinator:parse
                 "* my header :tag1:tag2:"
                 (list cltpt/org-mode::org-header))))
    result))

(test org-header-with-tags
  (is-match-tree
   (car (org-header-with-tags-func))
   '((:BEGIN 0 :END 23 :ID CLTPT/ORG-MODE:ORG-HEADER)
     ((:BEGIN 0 :END 23)
      ((:BEGIN 0 :END 1 :ID CLTPT/ORG-MODE::STARS))
      ((:BEGIN 1 :END 2))
      ((:BEGIN 2 :END 11 :ID CLTPT/ORG-MODE::TITLE))
      ((:BEGIN 11 :END 23 :ID CLTPT/ORG-MODE::TAGS)
       ((:BEGIN 0 :END 2))
       ((:BEGIN 2 :END 11)
        ((:BEGIN 0 :END 4 :ID CLTPT/ORG-MODE::TAG))
        ((:BEGIN 4 :END 5))
        ((:BEGIN 5 :END 9 :ID CLTPT/ORG-MODE::TAG)))
       ((:BEGIN 11 :END 12)))))))

;; comprehensive header test - more extensive with scheduling/closing
(defun org-header-comprehensive-test-func ()
  (let ((result (cltpt/combinator:scan-all-rules
                 nil
                 "* TODO my main header :test:here:noexport:
SCHEDULED: <2024-10-29 Tue 16:41:04>
CLOSED: [2024-10-29 Tue 16:41:03]
<2025-07-25 Fri 10:00:00>
:PROPERTIES:
:ID: my-id
:LAST_REPEAT: [2024-10-29 Tue 16:40:36]
:END:"
                 (list cltpt/org-mode::org-header))))
    result))

(test org-header-comprehensive
  (is-match-tree
   (car (org-header-comprehensive-test-func))
   '((:BEGIN 0 :END 209 :ID CLTPT/ORG-MODE:ORG-HEADER)
     ((:BEGIN 0 :END 42)
      ((:BEGIN 0 :END 1 :ID CLTPT/ORG-MODE::STARS))
      ((:BEGIN 1 :END 2))
      ((:BEGIN 2 :END 7)
       ((:BEGIN 0 :END 4 :ID CLTPT/ORG-MODE::TODO-KEYWORD))
       ((:BEGIN 4 :END 5)))
      ((:BEGIN 7 :END 21 :ID CLTPT/ORG-MODE::TITLE))
      ((:BEGIN 21 :END 42 :ID CLTPT/ORG-MODE::TAGS)
       ((:BEGIN 0 :END 2))
       ((:BEGIN 2 :END 20)
        ((:BEGIN 0 :END 4 :ID CLTPT/ORG-MODE::TAG))
        ((:BEGIN 4 :END 5))
        ((:BEGIN 5 :END 9 :ID CLTPT/ORG-MODE::TAG))
        ((:BEGIN 9 :END 10))
        ((:BEGIN 10 :END 18 :ID CLTPT/ORG-MODE::TAG)))
       ((:BEGIN 20 :END 21))))
     ((:BEGIN 42 :END 209)
      ((:BEGIN 0 :END 37)
       ((:BEGIN 0 :END 1))
       ((:BEGIN 1 :END 37)
        ((:BEGIN 0 :END 36)
         ((:BEGIN 0 :END 36)
          ((:BEGIN 0 :END 36 :ID CLTPT/ORG-MODE::ACTION-ACTIVE)
           ((:BEGIN 0 :END 9 :ID CLTPT/ORG-MODE::NAME))
           ((:BEGIN 9 :END 11))
           ((:BEGIN 11 :END 36 :ID CLTPT/ORG-MODE::TIMESTAMP)
            ((:BEGIN 0 :END 25 :ID CLTPT/ORG-MODE::BEGIN)
             ((:BEGIN 0 :END 1))
             ((:BEGIN 1 :END 5 :ID CLTPT/ORG-MODE::YEAR)
              ((:BEGIN 0 :END 4)))
             ((:BEGIN 5 :END 6))
             ((:BEGIN 6 :END 8 :ID CLTPT/ORG-MODE::MONTH)
              ((:BEGIN 0 :END 2)))
             ((:BEGIN 8 :END 9))
             ((:BEGIN 9 :END 11 :ID CLTPT/ORG-MODE::DAY)
              ((:BEGIN 0 :END 2)))
             ((:BEGIN 11 :END 15)
              ((:BEGIN 0 :END 1))
              ((:BEGIN 1 :END 4 :ID CLTPT/ORG-MODE::WEEKDAY)
               ((:BEGIN 0 :END 3))))
             ((:BEGIN 15 :END 24)
              ((:BEGIN 0 :END 1))
              ((:BEGIN 1 :END 3 :ID CLTPT/ORG-MODE::HOUR)
               ((:BEGIN 0 :END 2)))
              ((:BEGIN 3 :END 9)
               ((:BEGIN 0 :END 1))
               ((:BEGIN 1 :END 3 :ID CLTPT/ORG-MODE::MINUTE)
                ((:BEGIN 0 :END 2)))
               ((:BEGIN 3 :END 4))
               ((:BEGIN 4 :END 6 :ID SECOND)
                ((:BEGIN 0 :END 2)))))
             ((:BEGIN 24 :END 25)))))))))
      ((:BEGIN 37 :END 71)
       ((:BEGIN 0 :END 1))
       ((:BEGIN 1 :END 34)
        ((:BEGIN 0 :END 33)
         ((:BEGIN 0 :END 33)
          ((:BEGIN 0 :END 33 :ID CLTPT/ORG-MODE::ACTION-INACTIVE)
           ((:BEGIN 0 :END 6 :ID CLTPT/ORG-MODE::NAME))
           ((:BEGIN 6 :END 8))
           ((:BEGIN 8 :END 33 :ID CLTPT/ORG-MODE::TIMESTAMP)
            ((:BEGIN 0 :END 1))
            ((:BEGIN 1 :END 5 :ID CLTPT/ORG-MODE::YEAR)
             ((:BEGIN 0 :END 4)))
            ((:BEGIN 5 :END 6))
            ((:BEGIN 6 :END 8 :ID CLTPT/ORG-MODE::MONTH)
             ((:BEGIN 0 :END 2)))
            ((:BEGIN 8 :END 9))
            ((:BEGIN 9 :END 11 :ID CLTPT/ORG-MODE::DAY)
             ((:BEGIN 0 :END 2)))
            ((:BEGIN 11 :END 12))
            ((:BEGIN 12 :END 15 :ID CLTPT/ORG-MODE::WEEKDAY)
             ((:BEGIN 0 :END 3)))
            ((:BEGIN 15 :END 24)
             ((:BEGIN 0 :END 1))
             ((:BEGIN 1 :END 3 :ID CLTPT/ORG-MODE::HOUR)
              ((:BEGIN 0 :END 2)))
             ((:BEGIN 3 :END 4))
             ((:BEGIN 4 :END 6 :ID CLTPT/ORG-MODE::MINUTE)
              ((:BEGIN 0 :END 2)))
             ((:BEGIN 6 :END 7))
             ((:BEGIN 7 :END 9 :ID SECOND)
              ((:BEGIN 0 :END 2))))
            ((:BEGIN 24 :END 25))))))))
      ((:BEGIN 71 :END 97)
       ((:BEGIN 0 :END 1))
       ((:BEGIN 1 :END 26)
        ((:BEGIN 0 :END 25 :ID CLTPT/ORG-MODE::TODO-TIMESTAMP)
         ((:BEGIN 0 :END 25 :ID CLTPT/ORG-MODE::BEGIN)
          ((:BEGIN 0 :END 1))
          ((:BEGIN 1 :END 5 :ID CLTPT/ORG-MODE::YEAR)
           ((:BEGIN 0 :END 4)))
          ((:BEGIN 5 :END 6))
          ((:BEGIN 6 :END 8 :ID CLTPT/ORG-MODE::MONTH)
           ((:BEGIN 0 :END 2)))
          ((:BEGIN 8 :END 9))
          ((:BEGIN 9 :END 11 :ID CLTPT/ORG-MODE::DAY)
           ((:BEGIN 0 :END 2)))
          ((:BEGIN 11 :END 15)
           ((:BEGIN 0 :END 1))
           ((:BEGIN 1 :END 4 :ID CLTPT/ORG-MODE::WEEKDAY)
            ((:BEGIN 0 :END 3))))
          ((:BEGIN 15 :END 24)
           ((:BEGIN 0 :END 1))
           ((:BEGIN 1 :END 3 :ID CLTPT/ORG-MODE::HOUR)
            ((:BEGIN 0 :END 2)))
           ((:BEGIN 3 :END 9)
            ((:BEGIN 0 :END 1))
            ((:BEGIN 1 :END 3 :ID CLTPT/ORG-MODE::MINUTE)
             ((:BEGIN 0 :END 2)))
            ((:BEGIN 3 :END 4))
            ((:BEGIN 4 :END 6 :ID SECOND)
             ((:BEGIN 0 :END 2)))))
          ((:BEGIN 24 :END 25))))))
      ((:BEGIN 97 :END 167)
       ((:BEGIN 0 :END 1))
       ((:BEGIN 1 :END 70)
        ((:BEGIN 0 :END 69 :ID CLTPT/ORG-MODE:ORG-PROP-DRAWER)
         ((:BEGIN 0 :END 12 :ID CLTPT/ORG-MODE::DRAWER-OPEN-TAG))
         ((:BEGIN 13 :END 23 :ID CLTPT/ORG-MODE::DRAWER-ENTRY)
          ((:BEGIN 0 :END 1))
          ((:BEGIN 1 :END 3 :ID CLTPT/ORG-MODE::DRAWER-KEY))
          ((:BEGIN 3 :END 4))
          ((:BEGIN 4 :END 5))
          ((:BEGIN 5 :END 10 :ID CLTPT/ORG-MODE::DRAWER-VALUE)))
         ((:BEGIN 24 :END 63 :ID CLTPT/ORG-MODE::DRAWER-ENTRY)
          ((:BEGIN 0 :END 1))
          ((:BEGIN 1 :END 12 :ID CLTPT/ORG-MODE::DRAWER-KEY))
          ((:BEGIN 12 :END 13))
          ((:BEGIN 13 :END 14))
          ((:BEGIN 14 :END 39 :ID CLTPT/ORG-MODE::DRAWER-VALUE)))
         ((:BEGIN 64 :END 69)))))))
   ))

(defun org-timestamp-date-only-func ()
  (let ((result (cltpt/combinator:parse
                 "<2024-01-15 Mon>"
                 (list cltpt/org-mode::org-timestamp))))
    result))

(test org-timestamp-date-only
  (is-match-tree
   (car (org-timestamp-date-only-func))
   '((:BEGIN 0 :END 16 :ID CLTPT/ORG-MODE::ORG-TIMESTAMP)
     ((:BEGIN 0 :END 16 :ID CLTPT/ORG-MODE::BEGIN)
      ((:BEGIN 0 :END 1))
      ((:BEGIN 1 :END 5 :ID CLTPT/ORG-MODE::YEAR)
       ((:BEGIN 0 :END 4)))
      ((:BEGIN 5 :END 6))
      ((:BEGIN 6 :END 8 :ID CLTPT/ORG-MODE::MONTH)
       ((:BEGIN 0 :END 2)))
      ((:BEGIN 8 :END 9))
      ((:BEGIN 9 :END 11 :ID CLTPT/ORG-MODE::DAY)
       ((:BEGIN 0 :END 2)))
      ((:BEGIN 11 :END 15)
       ((:BEGIN 0 :END 1))
       ((:BEGIN 1 :END 4 :ID CLTPT/ORG-MODE::WEEKDAY)
        ((:BEGIN 0 :END 3))))
      ((:BEGIN 15 :END 16))))))

(defun org-timestamp-with-time-func ()
  (let ((result (cltpt/combinator:parse
                 "<2024-01-15 Mon 14:30:00>"
                 (list cltpt/org-mode::org-timestamp))))
    (match-tree-equal-p
     (car result)
     '((:BEGIN 0 :END 25 :ID CLTPT/ORG-MODE::ORG-TIMESTAMP)
       ((:BEGIN 0 :END 25 :ID CLTPT/ORG-MODE::BEGIN)
        ((:BEGIN 0 :END 1))
        ((:BEGIN 1 :END 5 :ID CLTPT/ORG-MODE::YEAR)
         ((:BEGIN 0 :END 4)))
        ((:BEGIN 5 :END 6))
        ((:BEGIN 6 :END 8 :ID CLTPT/ORG-MODE::MONTH)
         ((:BEGIN 0 :END 2)))
        ((:BEGIN 8 :END 9))
        ((:BEGIN 9 :END 11 :ID CLTPT/ORG-MODE::DAY)
         ((:BEGIN 0 :END 2)))
        ((:BEGIN 11 :END 15)
         ((:BEGIN 0 :END 1))
         ((:BEGIN 1 :END 4 :ID CLTPT/ORG-MODE::WEEKDAY)
          ((:BEGIN 0 :END 3))))
        ((:BEGIN 15 :END 24)
         ((:BEGIN 0 :END 1))
         ((:BEGIN 1 :END 3 :ID CLTPT/ORG-MODE::HOUR)
          ((:BEGIN 0 :END 2)))
         ((:BEGIN 3 :END 9)
          ((:BEGIN 0 :END 1))
          ((:BEGIN 1 :END 3 :ID CLTPT/ORG-MODE::MINUTE)
           ((:BEGIN 0 :END 2)))
          ((:BEGIN 3 :END 4))
          ((:BEGIN 4 :END 6 :ID SECOND)
           ((:BEGIN 0 :END 2)))))
        ((:BEGIN 24 :END 25)))))))

(test org-timestamp-with-time
  (fiveam:is (org-timestamp-with-time-func)))

(defun org-timestamp-with-repeater-func ()
  (let ((result (cltpt/combinator:parse
                 "<2024-01-15 Mon 14:30:00 +1w>"
                 (list cltpt/org-mode::org-timestamp))))
    result))

(test org-timestamp-with-repeater
  (is-match-tree
   (car (org-timestamp-with-repeater-func))
   '((:BEGIN 0 :END 29 :ID CLTPT/ORG-MODE::ORG-TIMESTAMP)
     ((:BEGIN 0 :END 29 :ID CLTPT/ORG-MODE::BEGIN)
      ((:BEGIN 0 :END 1))
      ((:BEGIN 1 :END 5 :ID CLTPT/ORG-MODE::YEAR)
       ((:BEGIN 0 :END 4)))
      ((:BEGIN 5 :END 6))
      ((:BEGIN 6 :END 8 :ID CLTPT/ORG-MODE::MONTH)
       ((:BEGIN 0 :END 2)))
      ((:BEGIN 8 :END 9))
      ((:BEGIN 9 :END 11 :ID CLTPT/ORG-MODE::DAY)
       ((:BEGIN 0 :END 2)))
      ((:BEGIN 11 :END 15)
       ((:BEGIN 0 :END 1))
       ((:BEGIN 1 :END 4 :ID CLTPT/ORG-MODE::WEEKDAY)
        ((:BEGIN 0 :END 3))))
      ((:BEGIN 15 :END 24)
       ((:BEGIN 0 :END 1))
       ((:BEGIN 1 :END 3 :ID CLTPT/ORG-MODE::HOUR)
        ((:BEGIN 0 :END 2)))
       ((:BEGIN 3 :END 9)
        ((:BEGIN 0 :END 1))
        ((:BEGIN 1 :END 3 :ID CLTPT/ORG-MODE::MINUTE)
         ((:BEGIN 0 :END 2)))
        ((:BEGIN 3 :END 4))
        ((:BEGIN 4 :END 6 :ID SECOND)
         ((:BEGIN 0 :END 2)))))
      ((:BEGIN 24 :END 28)
       ((:BEGIN 0 :END 2))
       ((:BEGIN 2 :END 3 :ID CLTPT/ORG-MODE::REPEAT-NUM)
        ((:BEGIN 0 :END 1)))
       ((:BEGIN 3 :END 4 :ID CLTPT/ORG-MODE::REPEAT-WORD)
        ((:BEGIN 0 :END 1))))
      ((:BEGIN 28 :END 29))))))

(defun org-timestamp-range-func ()
  (let ((result (cltpt/combinator:parse
                 "<2024-01-15 Mon>--<2024-01-16 Tue>"
                 (list cltpt/org-mode::org-timestamp))))
    (match-tree-equal-p
     (car result)
     '((:BEGIN 0 :END 34 :ID CLTPT/ORG-MODE::ORG-TIMESTAMP)
       ((:BEGIN 0 :END 16 :ID CLTPT/ORG-MODE::BEGIN)
        ((:BEGIN 0 :END 1))
        ((:BEGIN 1 :END 5 :ID CLTPT/ORG-MODE::YEAR)
         ((:BEGIN 0 :END 4)))
        ((:BEGIN 5 :END 6))
        ((:BEGIN 6 :END 8 :ID CLTPT/ORG-MODE::MONTH)
         ((:BEGIN 0 :END 2)))
        ((:BEGIN 8 :END 9))
        ((:BEGIN 9 :END 11 :ID CLTPT/ORG-MODE::DAY)
         ((:BEGIN 0 :END 2)))
        ((:BEGIN 11 :END 15)
         ((:BEGIN 0 :END 1))
         ((:BEGIN 1 :END 4 :ID CLTPT/ORG-MODE::WEEKDAY)
          ((:BEGIN 0 :END 3))))
        ((:BEGIN 15 :END 16)))
       ((:BEGIN 16 :END 18))
       ((:BEGIN 18 :END 34 :ID CLTPT/ORG-MODE::END)
        ((:BEGIN 0 :END 1))
        ((:BEGIN 1 :END 5 :ID CLTPT/ORG-MODE::YEAR)
         ((:BEGIN 0 :END 4)))
        ((:BEGIN 5 :END 6))
        ((:BEGIN 6 :END 8 :ID CLTPT/ORG-MODE::MONTH)
         ((:BEGIN 0 :END 2)))
        ((:BEGIN 8 :END 9))
        ((:BEGIN 9 :END 11 :ID CLTPT/ORG-MODE::DAY)
         ((:BEGIN 0 :END 2)))
        ((:BEGIN 11 :END 15)
         ((:BEGIN 0 :END 1))
         ((:BEGIN 1 :END 4 :ID CLTPT/ORG-MODE::WEEKDAY)
          ((:BEGIN 0 :END 3))))
        ((:BEGIN 15 :END 16)))))))

(test org-timestamp-range
  (fiveam:is (org-timestamp-range-func)))

(defun org-timestamp-comprehensive-date-only-func ()
  (match-tree-equal-p
   (cltpt/combinator:apply-rule nil cltpt/org-mode::org-timestamp (cltpt/reader:reader-from-string "<2023-12-28 Thu>") 0)
   '((:BEGIN 0 :END 16 :ID CLTPT/ORG-MODE::ORG-TIMESTAMP)
     ((:BEGIN 0 :END 16 :ID CLTPT/ORG-MODE::BEGIN)
      ((:BEGIN 0 :END 1))
      ((:BEGIN 1 :END 5 :ID CLTPT/ORG-MODE::YEAR)
       ((:BEGIN 0 :END 4)))
      ((:BEGIN 5 :END 6))
      ((:BEGIN 6 :END 8 :ID CLTPT/ORG-MODE::MONTH)
       ((:BEGIN 0 :END 2)))
      ((:BEGIN 8 :END 9))
      ((:BEGIN 9 :END 11 :ID CLTPT/ORG-MODE::DAY)
       ((:BEGIN 0 :END 2)))
      ((:BEGIN 11 :END 15)
       ((:BEGIN 0 :END 1))
       ((:BEGIN 1 :END 4 :ID CLTPT/ORG-MODE::WEEKDAY)
        ((:BEGIN 0 :END 3))))
      ((:BEGIN 15 :END 16))))))

(test org-timestamp-comprehensive-date-only
  (fiveam:is (org-timestamp-comprehensive-date-only-func)))

(defun org-timestamp-comprehensive-with-time-func ()
  (match-tree-equal-p
   (cltpt/combinator:apply-rule nil cltpt/org-mode::org-timestamp (cltpt/reader:reader-from-string "<2023-12-28 Thu 18:30:00>") 0)
   '((:BEGIN 0 :END 25 :ID CLTPT/ORG-MODE::ORG-TIMESTAMP)
     ((:BEGIN 0 :END 25 :ID CLTPT/ORG-MODE::BEGIN)
      ((:BEGIN 0 :END 1))
      ((:BEGIN 1 :END 5 :ID CLTPT/ORG-MODE::YEAR)
       ((:BEGIN 0 :END 4)))
      ((:BEGIN 5 :END 6))
      ((:BEGIN 6 :END 8 :ID CLTPT/ORG-MODE::MONTH)
       ((:BEGIN 0 :END 2)))
      ((:BEGIN 8 :END 9))
      ((:BEGIN 9 :END 11 :ID CLTPT/ORG-MODE::DAY)
       ((:BEGIN 0 :END 2)))
      ((:BEGIN 11 :END 15)
       ((:BEGIN 0 :END 1))
       ((:BEGIN 1 :END 4 :ID CLTPT/ORG-MODE::WEEKDAY)
        ((:BEGIN 0 :END 3))))
      ((:BEGIN 15 :END 24)
       ((:BEGIN 0 :END 1))
       ((:BEGIN 1 :END 3 :ID CLTPT/ORG-MODE::HOUR)
        ((:BEGIN 0 :END 2)))
       ((:BEGIN 3 :END 9)
        ((:BEGIN 0 :END 1))
        ((:BEGIN 1 :END 3 :ID CLTPT/ORG-MODE::MINUTE)
         ((:BEGIN 0 :END 2)))
        ((:BEGIN 3 :END 4))
        ((:BEGIN 4 :END 6 :ID SECOND)
         ((:BEGIN 0 :END 2)))))
      ((:BEGIN 24 :END 25))))))

(test org-timestamp-comprehensive-with-time
  (fiveam:is (org-timestamp-comprehensive-with-time-func)))

(defun org-timestamp-comprehensive-with-repeater-func ()
  (match-tree-equal-p
   (cltpt/combinator:apply-rule nil cltpt/org-mode::org-timestamp (cltpt/reader:reader-from-string "<2023-12-28 Thu 18:30:00 +1w>") 0)
   '((:BEGIN 0 :END 29 :ID CLTPT/ORG-MODE::ORG-TIMESTAMP)
     ((:BEGIN 0 :END 29 :ID CLTPT/ORG-MODE::BEGIN)
      ((:BEGIN 0 :END 1))
      ((:BEGIN 1 :END 5 :ID CLTPT/ORG-MODE::YEAR)
       ((:BEGIN 0 :END 4)))
      ((:BEGIN 5 :END 6))
      ((:BEGIN 6 :END 8 :ID CLTPT/ORG-MODE::MONTH)
       ((:BEGIN 0 :END 2)))
      ((:BEGIN 8 :END 9))
      ((:BEGIN 9 :END 11 :ID CLTPT/ORG-MODE::DAY)
       ((:BEGIN 0 :END 2)))
      ((:BEGIN 11 :END 15)
       ((:BEGIN 0 :END 1))
       ((:BEGIN 1 :END 4 :ID CLTPT/ORG-MODE::WEEKDAY)
        ((:BEGIN 0 :END 3))))
      ((:BEGIN 15 :END 24)
       ((:BEGIN 0 :END 1))
       ((:BEGIN 1 :END 3 :ID CLTPT/ORG-MODE::HOUR)
        ((:BEGIN 0 :END 2)))
       ((:BEGIN 3 :END 9)
        ((:BEGIN 0 :END 1))
        ((:BEGIN 1 :END 3 :ID CLTPT/ORG-MODE::MINUTE)
         ((:BEGIN 0 :END 2)))
        ((:BEGIN 3 :END 4))
        ((:BEGIN 4 :END 6 :ID SECOND)
         ((:BEGIN 0 :END 2)))))
      ((:BEGIN 24 :END 28)
       ((:BEGIN 0 :END 2))
       ((:BEGIN 2 :END 3 :ID CLTPT/ORG-MODE::REPEAT-NUM)
        ((:BEGIN 0 :END 1)))
       ((:BEGIN 3 :END 4 :ID CLTPT/ORG-MODE::REPEAT-WORD)
        ((:BEGIN 0 :END 1))))
      ((:BEGIN 28 :END 29))))))

(test org-timestamp-comprehensive-with-repeater
  (fiveam:is (org-timestamp-comprehensive-with-repeater-func)))

(defun org-timestamp-comprehensive-range-func ()
  (match-tree-equal-p
   (cltpt/combinator:apply-rule nil cltpt/org-mode::org-timestamp (cltpt/reader:reader-from-string "<2023-12-28 Thu 18:30:00 +1w>--<2023-12-28 Thu 19:00:00 +1w>") 0)
   '((:BEGIN 0 :END 60 :ID CLTPT/ORG-MODE::ORG-TIMESTAMP)
     ((:BEGIN 0 :END 29 :ID CLTPT/ORG-MODE::BEGIN)
      ((:BEGIN 0 :END 1))
      ((:BEGIN 1 :END 5 :ID CLTPT/ORG-MODE::YEAR)
       ((:BEGIN 0 :END 4)))
      ((:BEGIN 5 :END 6))
      ((:BEGIN 6 :END 8 :ID CLTPT/ORG-MODE::MONTH)
       ((:BEGIN 0 :END 2)))
      ((:BEGIN 8 :END 9))
      ((:BEGIN 9 :END 11 :ID CLTPT/ORG-MODE::DAY)
       ((:BEGIN 0 :END 2)))
      ((:BEGIN 11 :END 15)
       ((:BEGIN 0 :END 1))
       ((:BEGIN 1 :END 4 :ID CLTPT/ORG-MODE::WEEKDAY)
        ((:BEGIN 0 :END 3))))
      ((:BEGIN 15 :END 24)
       ((:BEGIN 0 :END 1))
       ((:BEGIN 1 :END 3 :ID CLTPT/ORG-MODE::HOUR)
        ((:BEGIN 0 :END 2)))
       ((:BEGIN 3 :END 9)
        ((:BEGIN 0 :END 1))
        ((:BEGIN 1 :END 3 :ID CLTPT/ORG-MODE::MINUTE)
         ((:BEGIN 0 :END 2)))
        ((:BEGIN 3 :END 4))
        ((:BEGIN 4 :END 6 :ID SECOND)
         ((:BEGIN 0 :END 2)))))
      ((:BEGIN 24 :END 28)
       ((:BEGIN 0 :END 2))
       ((:BEGIN 2 :END 3 :ID CLTPT/ORG-MODE::REPEAT-NUM)
        ((:BEGIN 0 :END 1)))
       ((:BEGIN 3 :END 4 :ID CLTPT/ORG-MODE::REPEAT-WORD)
        ((:BEGIN 0 :END 1))))
      ((:BEGIN 28 :END 29)))
     ((:BEGIN 29 :END 31))
     ((:BEGIN 31 :END 60 :ID CLTPT/ORG-MODE::END)
      ((:BEGIN 0 :END 1))
      ((:BEGIN 1 :END 5 :ID CLTPT/ORG-MODE::YEAR)
       ((:BEGIN 0 :END 4)))
      ((:BEGIN 5 :END 6))
      ((:BEGIN 6 :END 8 :ID CLTPT/ORG-MODE::MONTH)
       ((:BEGIN 0 :END 2)))
      ((:BEGIN 8 :END 9))
      ((:BEGIN 9 :END 11 :ID CLTPT/ORG-MODE::DAY)
       ((:BEGIN 0 :END 2)))
      ((:BEGIN 11 :END 15)
       ((:BEGIN 0 :END 1))
       ((:BEGIN 1 :END 4 :ID CLTPT/ORG-MODE::WEEKDAY)
        ((:BEGIN 0 :END 3))))
      ((:BEGIN 15 :END 24)
       ((:BEGIN 0 :END 1))
       ((:BEGIN 1 :END 3 :ID CLTPT/ORG-MODE::HOUR)
        ((:BEGIN 0 :END 2)))
       ((:BEGIN 3 :END 9)
        ((:BEGIN 0 :END 1))
        ((:BEGIN 1 :END 3 :ID CLTPT/ORG-MODE::MINUTE)
         ((:BEGIN 0 :END 2)))
        ((:BEGIN 3 :END 4))
        ((:BEGIN 4 :END 6 :ID SECOND)
         ((:BEGIN 0 :END 2)))))
      ((:BEGIN 24 :END 28)
       ((:BEGIN 0 :END 2))
       ((:BEGIN 2 :END 3 :ID CLTPT/ORG-MODE::REPEAT-NUM)
        ((:BEGIN 0 :END 1)))
       ((:BEGIN 3 :END 4 :ID CLTPT/ORG-MODE::REPEAT-WORD)
        ((:BEGIN 0 :END 1))))
      ((:BEGIN 28 :END 29))))))

(test org-timestamp-comprehensive-range
  (fiveam:is (org-timestamp-comprehensive-range-func)))

(defun org-single-timestamp-basic-func ()
  (let ((result (cltpt/combinator:parse
                 "<2024-01-15 Mon>"
                 (list cltpt/org-mode::*org-single-timestamp-rule*))))
    (match-tree-equal-p
     (car result)
     '((:BEGIN 0 :END 16)
       ((:BEGIN 0 :END 1))
       ((:BEGIN 1 :END 5 :ID CLTPT/ORG-MODE::YEAR)
        ((:BEGIN 0 :END 4)))
       ((:BEGIN 5 :END 6))
       ((:BEGIN 6 :END 8 :ID CLTPT/ORG-MODE::MONTH)
        ((:BEGIN 0 :END 2)))
       ((:BEGIN 8 :END 9))
       ((:BEGIN 9 :END 11 :ID CLTPT/ORG-MODE::DAY)
        ((:BEGIN 0 :END 2)))
       ((:BEGIN 11 :END 15)
        ((:BEGIN 0 :END 1))
        ((:BEGIN 1 :END 4 :ID CLTPT/ORG-MODE::WEEKDAY)
         ((:BEGIN 0 :END 3))))
       ((:BEGIN 15 :END 16))))))

(test org-single-timestamp-basic
  (fiveam:is (org-single-timestamp-basic-func)))

(defun org-link-simple-func ()
  (let ((result (cltpt/combinator:parse
                 "[[id:abc123]]"
                 (list cltpt/org-mode::org-link))))
    result))

(test org-link-simple
  (is-match-tree
   (car (org-link-simple-func))
   '((:BEGIN 0 :END 13 :ID CLTPT/ORG-MODE:ORG-LINK)
     ((:BEGIN 0 :END 13)
      ((:BEGIN 0 :END 2))
      ((:BEGIN 2 :END 4 :ID CLTPT/ORG-MODE::LINK-TYPE))
      ((:BEGIN 4 :END 5))
      ((:BEGIN 5 :END 11 :ID CLTPT/ORG-MODE::LINK-DEST))
      ((:BEGIN 11 :END 13))))))

(defun org-link-with-description-func ()
  (let ((result (cltpt/combinator:parse
                 "[[file:document.pdf][My Document]]"
                 (list cltpt/org-mode::org-link))))
    result))

(test org-link-with-description
  (is-match-tree
   (car (org-link-with-description-func))
   '((:BEGIN 0 :END 34 :ID CLTPT/ORG-MODE:ORG-LINK)
     ((:BEGIN 0 :END 34)
      ((:BEGIN 0 :END 2))
      ((:BEGIN 2 :END 6 :ID CLTPT/ORG-MODE::LINK-TYPE))
      ((:BEGIN 6 :END 7))
      ((:BEGIN 7 :END 19 :ID CLTPT/ORG-MODE::LINK-DEST))
      ((:BEGIN 19 :END 21))
      ((:BEGIN 21 :END 32 :ID CLTPT/ORG-MODE::LINK-DESC))
      ((:BEGIN 32 :END 34))))))

(defun org-link-no-type-func ()
  (let ((result (cltpt/combinator:parse
                 "[[document.pdf]]"
                 (list cltpt/org-mode::org-link))))
    result))

(test org-link-no-type
  (is-match-tree
   (car (org-link-no-type-func))
   '((:BEGIN 0 :END 16 :ID CLTPT/ORG-MODE:ORG-LINK)
     ((:BEGIN 0 :END 16)
      ((:BEGIN 0 :END 2))
      ((:BEGIN 2 :END 14 :ID CLTPT/ORG-MODE::LINK-DEST))
      ((:BEGIN 14 :END 16))))))

(defun org-web-link-basic-func ()
  (let ((result (cltpt/combinator:parse
                 "https://example.com/page"
                 (list cltpt/org-mode::web-link))))
    result
    ))

(test org-web-link-basic
  (is-match-tree
   (car (org-web-link-basic-func))
   '((:BEGIN 0 :END 24 :ID CLTPT/ORG-MODE::WEB-LINK)
     ((:BEGIN 0 :END 8)
      ((:BEGIN 0 :END 8)))
     ((:BEGIN 8 :END 24)))))

(defun org-inline-code-basic-func ()
  (let ((result (cltpt/combinator:parse
                 "~code here~"
                 (list cltpt/org-mode::org-inline-code))))
    result))

(test org-inline-code-basic
  (is-match-tree
   (car (org-inline-code-basic-func))
   '((:BEGIN 0 :END 11 :ID CLTPT/ORG-MODE::ORG-INLINE-CODE)
     ((:BEGIN 0 :END 1))
     ((:BEGIN 10 :END 11)))))

(defun org-keywords-basic-func ()
  (let ((result (cltpt/combinator:parse
                 " :hello-there :hello2"
                 (list cltpt/org-mode::*keywords-rule*))))
    result))

(test org-keywords-basic
  (is-match-tree
   (car (org-keywords-basic-func))
   '((:BEGIN 1 :END 21 :ID CLTPT/ORG-MODE::KEYWORDS)
     ((:BEGIN 0 :END 12)
      ((:BEGIN 0 :END 12 :ID CLTPT/ORG-MODE::KEYWORDS-ENTRY)
       ((:BEGIN 0 :END 12)
        ((:BEGIN 0 :END 1))
        ((:BEGIN 1 :END 12 :ID KEYWORD)))))
     ((:BEGIN 12 :END 13))
     ((:BEGIN 13 :END 20)
      ((:BEGIN 0 :END 7 :ID CLTPT/ORG-MODE::KEYWORDS-ENTRY)
       ((:BEGIN 0 :END 7)
        ((:BEGIN 0 :END 1))
        ((:BEGIN 1 :END 7 :ID KEYWORD))))))))

(defun org-italic-basic-func ()
  (let ((result (cltpt/combinator:parse
                 "/italic text/"
                 (list cltpt/org-mode::org-italic))))
    result))

(test org-italic-basic
  (is-match-tree
   (car (org-italic-basic-func))
   '((:BEGIN 0 :END 13 :MATCH "/italic text/")
     ((:BEGIN 0 :END 1 :MATCH "/"))
     ((:BEGIN 12 :END 13 :MATCH "/")))))

(defun org-prop-drawer-basic-test-func ()
  (let ((result (cltpt/combinator:parse
                 ":PROPERTIES:
:ID: my-id-123
:CUSTOM_ID: my-custom
:END:"
                 (list cltpt/org-mode::org-prop-drawer))))
    result))

(test org-prop-drawer-basic
  (is-match-tree
   (car (org-prop-drawer-basic-test-func))
   '((:BEGIN 0 :END 55 :ID CLTPT/ORG-MODE:ORG-PROP-DRAWER)
     ((:BEGIN 0 :END 12 :ID CLTPT/ORG-MODE::DRAWER-OPEN-TAG))
     ((:BEGIN 13 :END 27 :ID CLTPT/ORG-MODE::DRAWER-ENTRY)
      ((:BEGIN 0 :END 1))
      ((:BEGIN 1 :END 3 :ID CLTPT/ORG-MODE::DRAWER-KEY))
      ((:BEGIN 3 :END 4))
      ((:BEGIN 4 :END 5))
      ((:BEGIN 5 :END 14 :ID CLTPT/ORG-MODE::DRAWER-VALUE)))
     ((:BEGIN 28 :END 49 :ID CLTPT/ORG-MODE::DRAWER-ENTRY)
      ((:BEGIN 0 :END 1))
      ((:BEGIN 1 :END 10 :ID CLTPT/ORG-MODE::DRAWER-KEY))
      ((:BEGIN 10 :END 11))
      ((:BEGIN 11 :END 12))
      ((:BEGIN 12 :END 21 :ID CLTPT/ORG-MODE::DRAWER-VALUE)))
     ((:BEGIN 50 :END 55)))))

(defun org-prop-drawer-empty-test-func ()
  (let ((result (cltpt/combinator:parse
                 ":PROPERTIES:
:END:"
                 (list cltpt/org-mode::org-prop-drawer))))
    result))

(test org-prop-drawer-empty
  (is-match-tree
   (car (org-prop-drawer-empty-test-func))
   '((:BEGIN 0 :END 18 :ID CLTPT/ORG-MODE:ORG-PROP-DRAWER)
     ((:BEGIN 0 :END 12 :ID CLTPT/ORG-MODE::DRAWER-OPEN-TAG))
     ((:BEGIN 13 :END 18)))))

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
  (is-match-tree
   (car (org-drawer-basic-func))
   '((:BEGIN 0 :END 54 :ID CLTPT/ORG-MODE:ORG-DRAWER)
     ((:BEGIN 0 :END 9 :ID CLTPT/ORG-MODE::DRAWER-OPEN-TAG)
      ((:BEGIN 0 :END 1))
      ((:BEGIN 1 :END 8)
       ((:BEGIN 0 :END 7)))
      ((:BEGIN 8 :END 9)))
     ((:BEGIN 10 :END 48 :ID CLTPT/ORG-MODE:ORG-LIST)
      ((:BEGIN 0 :END 38 :ID CLTPT/ORG-MODE::LIST-ITEM)
       ((:BEGIN 0 :END 1 :ID CLTPT/ORG-MODE::LIST-ITEM-BULLET))
       ((:BEGIN 2 :END 38 :ID CLTPT/ORG-MODE::LIST-ITEM-CONTENT))))
     ((:BEGIN 49 :END 54 :ID CLTPT/ORG-MODE::DRAWER-CLOSE-TAG)))))

(defun org-src-block-basic-func ()
  (let ((result (cltpt/combinator:parse
                 "#+begin_src python
  print('hello')
#+end_src"
                 (list cltpt/org-mode::org-src-block))))
    result))

(test org-src-block-basic
  (is-match-tree
   (car (org-src-block-basic-func))
   '((:BEGIN 0 :END 45 :ID CLTPT/ORG-MODE:ORG-SRC-BLOCK)
     ((:BEGIN 0 :END 45)
      ((:BEGIN 0 :END 18 :ID CLTPT/ORG-MODE::BEGIN)
       ((:BEGIN 0 :END 18)
        ((:BEGIN 0 :END 11 :ID CLTPT/ORG-MODE::OPEN-TAG))
        ((:BEGIN 11 :END 12))
        ((:BEGIN 12 :END 18 :ID CLTPT/ORG-MODE::LANG))))
      ((:BEGIN 36 :END 45 :ID CLTPT/ORG-MODE::END))))))

(defun org-src-block-with-options-func ()
  (let ((result (cltpt/combinator:parse
                 "#+begin_src python :results %'(:name link-dest (cltpt/combinator:atleast-one-discard (cltpt/combinator:any))) :reconstruct %cltpt/org-mode::org-link
print('hello')
#+end_src"
                 (list cltpt/org-mode::org-src-block))))
    result))

(test org-src-block-with-options
  (is-match-tree
   (car (org-src-block-with-options-func))
   '((:BEGIN 0 :END 173 :ID CLTPT/ORG-MODE:ORG-SRC-BLOCK)
     ((:BEGIN 0 :END 173)
      ((:BEGIN 0 :END 148 :ID CLTPT/ORG-MODE::BEGIN)
       ((:BEGIN 0 :END 148)
        ((:BEGIN 0 :END 11 :ID CLTPT/ORG-MODE::OPEN-TAG))
        ((:BEGIN 11 :END 12))
        ((:BEGIN 12 :END 18 :ID CLTPT/ORG-MODE::LANG))
        ((:BEGIN 18 :END 19))
        ((:BEGIN 19 :END 148 :ID CLTPT/ORG-MODE::KEYWORDS)
         ((:BEGIN 0 :END 90)
          ((:BEGIN 0 :END 90 :ID CLTPT/ORG-MODE::KEYWORDS-ENTRY)
           ((:BEGIN 0 :END 1))
           ((:BEGIN 1 :END 8 :ID KEYWORD))
           ((:BEGIN 8 :END 9))
           ((:BEGIN 9 :END 90 :ID CLTPT/BASE:POST-LEXER-TEXT-MACRO)
            ((:BEGIN 0 :END 1))
            ((:BEGIN 1 :END 81 :ID CLTPT/BASE::LISP-CODE)))))
         ((:BEGIN 90 :END 91))
         ((:BEGIN 91 :END 129)
          ((:BEGIN 0 :END 38 :ID CLTPT/ORG-MODE::KEYWORDS-ENTRY)
           ((:BEGIN 0 :END 1))
           ((:BEGIN 1 :END 12 :ID KEYWORD))
           ((:BEGIN 12 :END 13))
           ((:BEGIN 13 :END 38 :ID CLTPT/BASE:POST-LEXER-TEXT-MACRO)
            ((:BEGIN 0 :END 1))
            ((:BEGIN 1 :END 25 :ID CLTPT/BASE::LISP-CODE))))))))
      ((:BEGIN 164 :END 173 :ID CLTPT/ORG-MODE::END))))))

(defun org-src-block-with-name-func ()
  (let ((result (cltpt/combinator:parse
                 "#+name: my-code-block
#+begin_src lisp
(+ 1 2)
#+end_src"
                 (list cltpt/org-mode::org-src-block))))
    result))

(test org-src-block-with-name
  (is-match-tree
   (car (org-src-block-with-name-func))
   '((:BEGIN 22 :END 56 :ID CLTPT/ORG-MODE:ORG-SRC-BLOCK)
     ((:BEGIN 0 :END 34)
      ((:BEGIN 0 :END 16 :ID CLTPT/ORG-MODE::BEGIN)
       ((:BEGIN 0 :END 16)
        ((:BEGIN 0 :END 11 :ID CLTPT/ORG-MODE::OPEN-TAG))
        ((:BEGIN 11 :END 12))
        ((:BEGIN 12 :END 16 :ID CLTPT/ORG-MODE::LANG))))
      ((:BEGIN 25 :END 34 :ID CLTPT/ORG-MODE::END))))))

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
  (is-match-tree
   (car (org-src-block-comprehensive-with-results-func))
   '((:BEGIN 1 :END 152 :ID CLTPT/ORG-MODE:ORG-SRC-BLOCK)
     ((:BEGIN 0 :END 151)
      ((:BEGIN 0 :END 103)
       ((:BEGIN 0 :END 34 :ID CLTPT/ORG-MODE::BEGIN)
        ((:BEGIN 0 :END 34)
         ((:BEGIN 0 :END 11 :ID CLTPT/ORG-MODE::OPEN-TAG))
         ((:BEGIN 11 :END 12))
         ((:BEGIN 12 :END 18 :ID CLTPT/ORG-MODE::LANG))
         ((:BEGIN 18 :END 19))
         ((:BEGIN 19 :END 34 :ID CLTPT/ORG-MODE::KEYWORDS)
          ((:BEGIN 0 :END 15)
           ((:BEGIN 0 :END 15 :ID CLTPT/ORG-MODE::KEYWORDS-ENTRY)
            ((:BEGIN 0 :END 8)
             ((:BEGIN 0 :END 1))
             ((:BEGIN 1 :END 8 :ID KEYWORD)))
            ((:BEGIN 8 :END 15)
             ((:BEGIN 0 :END 1))
             ((:BEGIN 1 :END 7 :ID CLTPT/ORG-MODE::VALUE))))))))
       ((:BEGIN 94 :END 103 :ID CLTPT/ORG-MODE::END)))
      ((:BEGIN 103 :END 104))
      ((:BEGIN 104 :END 105))
      ((:BEGIN 105 :END 151 :ID CLTPT/ORG-MODE::RESULTS)
       ((:BEGIN 0 :END 10)
        ((:BEGIN 0 :END 10)))
       ((:BEGIN 10 :END 11))
       ((:BEGIN 11 :END 46 :ID CLTPT/ORG-MODE::RESULTS-CONTENT)
        ((:BEGIN 0 :END 35)
         ((:BEGIN 0 :END 10)
          ((:BEGIN 0 :END 10 :ID CLTPT/ORG-MODE::OUTPUT-LINE)
           ((:BEGIN 0 :END 10)
            ((:BEGIN 0 :END 2))
            ((:BEGIN 2 :END 10)))))
         ((:BEGIN 10 :END 11))
         ((:BEGIN 11 :END 22)
          ((:BEGIN 0 :END 11 :ID CLTPT/ORG-MODE::OUTPUT-LINE)
           ((:BEGIN 0 :END 11)
            ((:BEGIN 0 :END 2))
            ((:BEGIN 2 :END 11)))))
         ((:BEGIN 22 :END 23))
         ((:BEGIN 23 :END 29)
          ((:BEGIN 0 :END 6 :ID CLTPT/ORG-MODE::OUTPUT-LINE)
           ((:BEGIN 0 :END 6)
            ((:BEGIN 0 :END 2))
            ((:BEGIN 2 :END 6)))))
         ((:BEGIN 29 :END 30))
         ((:BEGIN 30 :END 35)
          ((:BEGIN 0 :END 5 :ID CLTPT/ORG-MODE::OUTPUT-LINE)
           ((:BEGIN 0 :END 5)
            ((:BEGIN 0 :END 2))
            ((:BEGIN 2 :END 5))))))))))))


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
  (is-match-tree
   (car (org-src-block-comprehensive-with-file-results-func))
   '((:BEGIN 1 :END 145 :ID CLTPT/ORG-MODE:ORG-SRC-BLOCK)
     ((:BEGIN 0 :END 144)
      ((:BEGIN 0 :END 57)
       ((:BEGIN 0 :END 34 :ID CLTPT/ORG-MODE::BEGIN)
        ((:BEGIN 0 :END 34)
         ((:BEGIN 0 :END 11 :ID CLTPT/ORG-MODE::OPEN-TAG))
         ((:BEGIN 11 :END 12))
         ((:BEGIN 12 :END 18 :ID CLTPT/ORG-MODE::LANG))
         ((:BEGIN 18 :END 19))
         ((:BEGIN 19 :END 34 :ID CLTPT/ORG-MODE::KEYWORDS)
          ((:BEGIN 0 :END 15)
           ((:BEGIN 0 :END 15 :ID CLTPT/ORG-MODE::KEYWORDS-ENTRY)
            ((:BEGIN 0 :END 8)
             ((:BEGIN 0 :END 1))
             ((:BEGIN 1 :END 8 :ID KEYWORD)))
            ((:BEGIN 8 :END 15)
             ((:BEGIN 0 :END 1))
             ((:BEGIN 1 :END 7 :ID CLTPT/ORG-MODE::VALUE))))))))
       ((:BEGIN 48 :END 57 :ID CLTPT/ORG-MODE::END)))
      ((:BEGIN 57 :END 58))
      ((:BEGIN 58 :END 59))
      ((:BEGIN 59 :END 144 :ID CLTPT/ORG-MODE::RESULTS)
       ((:BEGIN 0 :END 52)
        ((:BEGIN 0 :END 52)
         ((:BEGIN 0 :END 10))
         ((:BEGIN 10 :END 50))
         ((:BEGIN 50 :END 52))))
       ((:BEGIN 52 :END 53))
       ((:BEGIN 53 :END 85 :ID CLTPT/ORG-MODE::RESULTS-CONTENT)
        ((:BEGIN 0 :END 32)
         ((:BEGIN 0 :END 32 :ID CLTPT/ORG-MODE:ORG-LINK)
          ((:BEGIN 0 :END 32)
           ((:BEGIN 0 :END 2))
           ((:BEGIN 2 :END 6 :ID CLTPT/ORG-MODE::LINK-TYPE))
           ((:BEGIN 6 :END 7))
           ((:BEGIN 7 :END 30 :ID CLTPT/ORG-MODE::LINK-DEST))
           ((:BEGIN 30 :END 32)))))))))))

(defun org-export-block-html-func ()
  (let ((result (cltpt/combinator:parse
                 "#+begin_export html
<div>Custom HTML</div>
#+end_export"
                 (list cltpt/org-mode::org-export-block))))
    result))

(test org-export-block-html
  (is-match-tree
   (car (org-export-block-html-func))
   '((:BEGIN 0 :END 55 :ID CLTPT/ORG-MODE:ORG-EXPORT-BLOCK)
     ((:BEGIN 0 :END 19 :ID CLTPT/ORG-MODE::BEGIN)
      ((:BEGIN 0 :END 19)
       ((:BEGIN 0 :END 14 :ID CLTPT/ORG-MODE::OPEN-TAG))
       ((:BEGIN 14 :END 15))
       ((:BEGIN 15 :END 19 :ID CLTPT/ORG-MODE::LANG))))
     ((:BEGIN 43 :END 55 :ID CLTPT/ORG-MODE::END)))))

(defun org-export-block-latex-func ()
  (let ((result (cltpt/combinator:parse
                 "#+begin_export latex
\\textbf{Bold text}
#+end_export"
                 (list cltpt/org-mode::org-export-block))))
    result))

(test org-export-block-latex
  (is-match-tree
   (car (org-export-block-latex-func))
   '((:BEGIN 0 :END 52 :ID CLTPT/ORG-MODE:ORG-EXPORT-BLOCK)
     ((:BEGIN 0 :END 20 :ID CLTPT/ORG-MODE::BEGIN)
      ((:BEGIN 0 :END 20)
       ((:BEGIN 0 :END 14 :ID CLTPT/ORG-MODE::OPEN-TAG))
       ((:BEGIN 14 :END 15))
       ((:BEGIN 15 :END 20 :ID CLTPT/ORG-MODE::LANG))))
     ((:BEGIN 40 :END 52 :ID CLTPT/ORG-MODE::END)))))

(defun org-block-example-func ()
  (let ((result (cltpt/combinator:parse
                 "#+begin_example: test"
                 (list cltpt/org-mode::org-keyword))))
    result))

(test org-block-example
  (is-match-tree
   (car (org-block-example-func))
   '((:BEGIN 0 :END 21 :ID CLTPT/ORG-MODE:ORG-KEYWORD)
     ((:BEGIN 0 :END 16)
      ((:BEGIN 0 :END 2))
      ((:BEGIN 2 :END 15 :ID KEYWORD))
      ((:BEGIN 15 :END 16)))
     ((:BEGIN 16 :END 17))
     ((:BEGIN 17 :END 21)
      ((:BEGIN 0 :END 4 :ID CLTPT/ORG-MODE::VALUE))))))

(defun org-block-with-keywords-func ()
  (let ((result (cltpt/combinator:parse
                 "#+name: my-block"
                 (list cltpt/org-mode::org-keyword))))
    result))

(test org-block-with-keywords
  (is-match-tree
   (car (org-block-with-keywords-func))
   '((:BEGIN 0 :END 16 :ID CLTPT/ORG-MODE:ORG-KEYWORD)
     ((:BEGIN 0 :END 7)
      ((:BEGIN 0 :END 2))
      ((:BEGIN 2 :END 6 :ID KEYWORD))
      ((:BEGIN 6 :END 7)))
     ((:BEGIN 7 :END 8))
     ((:BEGIN 8 :END 16)
      ((:BEGIN 0 :END 8 :ID CLTPT/ORG-MODE::VALUE))))))

(defun org-babel-results-simple-func ()
  (let ((result (cltpt/combinator:parse
                 "#+RESULTS:
: 42"
                 (list cltpt/org-mode::*org-babel-results-rule*))))
    result))

(test org-babel-results-simple
  (is-match-tree
   (car (org-babel-results-simple-func))
   '((:BEGIN 0 :END 15 :ID CLTPT/ORG-MODE::RESULTS)
     ((:BEGIN 0 :END 10)
      ((:BEGIN 0 :END 10)))
     ((:BEGIN 10 :END 11))
     ((:BEGIN 11 :END 15 :ID CLTPT/ORG-MODE::RESULTS-CONTENT)
      ((:BEGIN 0 :END 4)
       ((:BEGIN 0 :END 4)
        ((:BEGIN 0 :END 4 :ID CLTPT/ORG-MODE::OUTPUT-LINE)
         ((:BEGIN 0 :END 4)
          ((:BEGIN 0 :END 2))
          ((:BEGIN 2 :END 4))))))))))

(defun org-babel-results-with-hash-func ()
  (let ((result (cltpt/combinator:parse
                 "#+RESULTS[abc123def]:
: output here"
                 (list cltpt/org-mode::*org-babel-results-rule*))))
    result))

(test org-babel-results-with-hash
  (is-match-tree
   (car (org-babel-results-with-hash-func))
   '((:BEGIN 0 :END 35 :ID CLTPT/ORG-MODE::RESULTS)
     ((:BEGIN 0 :END 21)
      ((:BEGIN 0 :END 21)
       ((:BEGIN 0 :END 10))
       ((:BEGIN 10 :END 19))
       ((:BEGIN 19 :END 21))))
     ((:BEGIN 21 :END 22))
     ((:BEGIN 22 :END 35 :ID CLTPT/ORG-MODE::RESULTS-CONTENT)
      ((:BEGIN 0 :END 13)
       ((:BEGIN 0 :END 13)
        ((:BEGIN 0 :END 13 :ID CLTPT/ORG-MODE::OUTPUT-LINE)
         ((:BEGIN 0 :END 13)
          ((:BEGIN 0 :END 2))
          ((:BEGIN 2 :END 13))))))))))


(defun org-latex-env-basic-func ()
  (let ((result (cltpt/combinator:parse
                 "#+latex: \\begin{equation}"
                 (list cltpt/org-mode::org-keyword))))
    result))

(test org-latex-env-basic
  (is-match-tree
   (car (org-latex-env-basic-func))
   '((:BEGIN 0 :END 25 :ID CLTPT/ORG-MODE:ORG-KEYWORD)
     ((:BEGIN 0 :END 8)
      ((:BEGIN 0 :END 2))
      ((:BEGIN 2 :END 7 :ID KEYWORD))
      ((:BEGIN 7 :END 8)))
     ((:BEGIN 8 :END 9))
     ((:BEGIN 9 :END 25)
      ((:BEGIN 0 :END 16 :ID CLTPT/ORG-MODE::VALUE))))))

(defun latex-env-parse-test-1 ()
  (cltpt/combinator::parse
   "
\\begin{gather}
some math here
\\end{gather}
"
   (list
    cltpt/latex:latex-env)))

(test latex-env-parse-test-1
  (is-match-tree
   (car (latex-env-parse-test-1))
   '((:BEGIN 1 :END 43 :ID CLTPT/LATEX:LATEX-ENV)
     ((:BEGIN 0 :END 14 :ID CLTPT/LATEX::OPEN-TAG)
      ((:BEGIN 0 :END 7))
      ((:BEGIN 7 :END 13))
      ((:BEGIN 13 :END 14)))
     ((:BEGIN 30 :END 42 :ID CLTPT/LATEX::CLOSE-TAG)
      ((:BEGIN 0 :END 5))
      ((:BEGIN 5 :END 11))
      ((:BEGIN 11 :END 12))))))

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
  (is-match-tree
   (car (cltpt/tests/org-mode::inline-latex-test-func))
   '((:BEGIN 6 :END 26)
     ((:BEGIN 0 :END 2 :ID OPENING))
     ((:BEGIN 7 :END 18 :ID KEYWORD)
      ((:BEGIN 0 :END 5)
       ((:BEGIN 0 :END 5)))
      ((:BEGIN 5 :END 11)))
     ((:BEGIN 18 :END 20 :ID ENDING)))))

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
  (is-match-tree
   (car (keywords-test-1))
   '((:BEGIN 1 :END 32 :ID CLTPT/ORG-MODE::KEYWORDS)
     ((:BEGIN 0 :END 17)
      ((:BEGIN 0 :END 17 :ID CLTPT/ORG-MODE::KEYWORDS-ENTRY)
       ((:BEGIN 0 :END 12)
        ((:BEGIN 0 :END 1))
        ((:BEGIN 1 :END 12 :ID KEYWORD)))
       ((:BEGIN 12 :END 17)
        ((:BEGIN 0 :END 1))
        ((:BEGIN 1 :END 5 :ID CLTPT/ORG-MODE::VALUE)))))
     ((:BEGIN 17 :END 18))
     ((:BEGIN 18 :END 31)
      ((:BEGIN 0 :END 13 :ID CLTPT/ORG-MODE::KEYWORDS-ENTRY)
       ((:BEGIN 0 :END 7)
        ((:BEGIN 0 :END 1))
        ((:BEGIN 1 :END 7 :ID KEYWORD)))
       ((:BEGIN 7 :END 13)
        ((:BEGIN 0 :END 1))
        ((:BEGIN 1 :END 6 :ID CLTPT/ORG-MODE::VALUE))))))))

(defun test-org-latex-env ()
  (cltpt/combinator:parse
   "
#+name: test-name
\\begin{equation}
my equation here
\\end{equation}
"
   (list cltpt/org-mode::org-latex-env)))

(test test-org-latex-env
  (is-match-tree
   (car (test-org-latex-env))
   '((:BEGIN 1 :END 67 :ID CLTPT/ORG-MODE:ORG-LATEX-ENV)
     ((:BEGIN 0 :END 17)
      ((:BEGIN 0 :END 17 :ID CLTPT/ORG-MODE:ORG-KEYWORD)
       ((:BEGIN 0 :END 7)
        ((:BEGIN 0 :END 2))
        ((:BEGIN 2 :END 6 :ID KEYWORD))
        ((:BEGIN 6 :END 7)))
       ((:BEGIN 7 :END 8))
       ((:BEGIN 8 :END 17)
        ((:BEGIN 0 :END 9 :ID CLTPT/ORG-MODE::VALUE)))))
     ((:BEGIN 17 :END 18))
     ((:BEGIN 18 :END 66 :ID CLTPT/ORG-MODE::LATEX-ENV-1)
      ((:BEGIN 0 :END 16 :ID CLTPT/LATEX::OPEN-TAG)
       ((:BEGIN 0 :END 7))
       ((:BEGIN 7 :END 15))
       ((:BEGIN 15 :END 16)))
      ((:BEGIN 34 :END 48 :ID CLTPT/LATEX::CLOSE-TAG)
       ((:BEGIN 0 :END 5))
       ((:BEGIN 5 :END 13))
       ((:BEGIN 13 :END 14)))))))

(defun test-org-keyword ()
  (cltpt/combinator:parse
   "
 #+title: add vid to github readme
 #+date: <2024-04-04 Thu 15:52:09>
 #+filetags: 
 #+identifier: 1712235129
 "
   (list cltpt/org-mode::org-keyword)))

(test test-org-keyword
  (let ((parser-result (test-org-keyword)))
    (fiveam:is
     (match-tree-equal-p
      (car parser-result)
      '((:BEGIN 2 :END 35 :ID CLTPT/ORG-MODE:ORG-KEYWORD)
        ((:BEGIN 0 :END 8)
         ((:BEGIN 0 :END 2))
         ((:BEGIN 2 :END 7 :ID KEYWORD))
         ((:BEGIN 7 :END 8)))
        ((:BEGIN 8 :END 9))
        ((:BEGIN 9 :END 33)
         ((:BEGIN 0 :END 24 :ID CLTPT/ORG-MODE::VALUE))))))))

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
  (let ((content (uiop:read-file-string "tests/data/comprehensive-org-test.org")))
    (cltpt/base:parse cltpt/org-mode:*org-mode* content)))

(defun create-expected-types-tree ()
  '(cltpt/org-mode::org-document
    ;; 7 keywords at the top level
    (cltpt/org-mode::org-keyword)
    (cltpt/org-mode::org-keyword)
    (cltpt/org-mode::org-keyword)
    (cltpt/org-mode::org-keyword)
    (cltpt/org-mode::org-keyword)
    (cltpt/org-mode::org-keyword)
    (cltpt/org-mode::org-keyword)
    ;; main header
    (cltpt/org-mode::org-header
     (cltpt/org-mode::org-header
      (cltpt/org-mode::org-emph)
      (cltpt/org-mode::org-italic)
      (cltpt/org-mode::org-inline-code))
     ;; links and images section
     (cltpt/org-mode::org-header
      (cltpt/org-mode::org-list
       (cltpt/org-mode::web-link)
       (cltpt/org-mode::org-link)
       (cltpt/org-mode::org-link)
       (cltpt/org-mode::org-link)))
     ;; lists section (3 sub-headers)
     (cltpt/org-mode::org-header
      ;; unordered lists
      (cltpt/org-mode::org-header
       (cltpt/org-mode::org-list
        (cltpt/org-mode::org-list
         (cltpt/org-mode::org-list))))
      ;; ordered lists
      (cltpt/org-mode::org-header
       (cltpt/org-mode::org-list
        (cltpt/org-mode::org-list)))
      ;; description lists
      (cltpt/org-mode::org-header
       (cltpt/org-mode::org-list)))
     ;; code section (3 sub-headers)
     (cltpt/org-mode::org-header
      ;; python
      (cltpt/org-mode::org-header
       (cltpt/org-mode::org-src-block))
      ;; javascript
      (cltpt/org-mode::org-header
       (cltpt/org-mode::org-src-block))
      ;; latex
      (cltpt/org-mode::org-header
       (cltpt/org-mode::org-src-block)))
     ;; math section
     (cltpt/org-mode::org-header
      (cltpt/org-mode::org-header)
      (cltpt/org-mode::org-header
       (cltpt/org-mode::org-latex-env
        (cltpt/org-mode::org-keyword))
       (cltpt/org-mode::org-latex-env
        (cltpt/org-mode::org-keyword))))
     ;; tables section (2 sub-headers)
     (cltpt/org-mode::org-header
      (cltpt/org-mode::org-header
       (cltpt/org-mode::org-table))
      (cltpt/org-mode::org-header
       (cltpt/org-mode::org-table)))
     ;; timestamps section (2 sub-headers)
     (cltpt/org-mode::org-header
      ;; deadline
      (cltpt/org-mode::org-header
       (cltpt/org-mode::org-list))
      ;; timestamps
      (cltpt/org-mode::org-header
       (cltpt/org-mode::org-list)))
     ;; tags and tasks section (3 sub-headers)
     (cltpt/org-mode::org-header
      ;; task with tags
      (cltpt/org-mode::org-header)
      ;; TODO with properties drawer
      (cltpt/org-mode::org-header
       (cltpt/org-mode::org-prop-drawer))
      ;; DONE with properties drawer and list
      (cltpt/org-mode::org-header
       (cltpt/org-mode::org-prop-drawer)
       (cltpt/org-mode::org-list)))
     ;; blocks and quotes section (4 sub-headers)
     (cltpt/org-mode::org-header
      (cltpt/org-mode::org-header
       (cltpt/org-mode::org-block))
      (cltpt/org-mode::org-header
       (cltpt/org-mode::org-block))
      (cltpt/org-mode::org-header
       (cltpt/org-mode::org-block))
      (cltpt/org-mode::org-header
       (cltpt/org-mode::org-export-block)
       (cltpt/org-mode::org-export-block)))
     ;; comments section (2 sub-headers)
     (cltpt/org-mode::org-header
      (cltpt/org-mode::org-header
       (cltpt/org-mode::org-comment))
      (cltpt/org-mode::org-header))
     ;; advanced features section (3 sub-headers)
     (cltpt/org-mode::org-header
      (cltpt/org-mode::org-header
       (cltpt/org-mode::org-keyword))
      (cltpt/org-mode::org-header
       (cltpt/org-mode::org-keyword))
      (cltpt/org-mode::org-header
       (cltpt/org-mode::org-link)))
     ;; target search section
     (cltpt/org-mode::org-header)
     ;; final section
     (cltpt/org-mode::org-header
      (cltpt/org-mode::org-emph)))))

(defun test-comprehensive-org-document-parse ()
  (let* ((doc (test-comprehensive-org-document-func))
         (expected-tree (create-expected-types-tree))
         (errors (compare-tree-types doc expected-tree)))
    (if errors
        (progn
          (format t "~&tree type validation failed:~%")
          (loop for error in errors
                do (format t "  ~a~%" error))
          nil)
        t)))

(test comprehensive-org-document-structure-validation
  (let ((doc (test-comprehensive-org-document-func)))
    (format t "~&actual tree structure (first 3 levels):~%")
    (cltpt/tree:tree-show doc)
    (fiveam:is (test-comprehensive-org-document-parse))))

(defun test-agenda-rendering-func ()
  "parse test.org, build an agenda, and render it for a fixed date range."
  (let* ((rmr (cltpt/roam:roamer-from-files
               '((:path ("./tests/test.org")
                  :format "org-mode"))))
         (agenda (cltpt/agenda:from-roamer rmr))
         (begin-ts (local-time:encode-timestamp 0 0 0 0 25 7 2025))
         (end-ts (local-time:encode-timestamp 0 0 0 0 1 8 2025)))
    (cltpt/agenda:render-agenda agenda :begin-ts begin-ts :end-ts end-ts)))

(test test-agenda-rendering
  (let ((actual (test-agenda-rendering-func))
        (expected (uiop:read-file-string "tests/data/test-org-expected-agenda.txt")))
    (is (string=+diff
         actual
         expected
         "agenda rendering of test.org should match expected output")
        "agenda rendering of test.org should match expected output")))

(test test-agenda-options
  "test first-repeat-only and include-done together."
  (let* ((rmr (cltpt/roam:roamer-from-files
               '((:path ("./tests/test.org")
                  :format "org-mode"))))
         (agenda (cltpt/agenda:from-roamer rmr))
         (begin-ts (local-time:encode-timestamp 0 0 0 0 25 7 2025))
         (end-ts (local-time:encode-timestamp 0 0 0 0 1 8 2025))
         (actual (cltpt/agenda:render-agenda agenda
                                             :begin-ts begin-ts
                                             :end-ts end-ts
                                             :first-repeat-only t
                                             :include-done t)))
    (is (string=+diff
         actual
         "├─ Friday 25 July 2025
│ ├─ 00:00
│ ├─ 02:00
│ ├─ 04:00
│ ├─ 06:00
│ ├─ 08:00
│ ├─ 10:00
│ ├─ 12:00
│ ├─ 14:00
│ ├─ 16:00
│ ├─ 18:00
│ ├─ 20:00
│ └─ 22:00
├─ Saturday 26 July 2025
│ └─ START: (TODO) 10:55 header my secondary header     :tag1:tag2:important:
├─ Sunday 27 July 2025
│ ├─ START: (TODO) 10:55 header do something                       :noexport:
│ └─ START: (TODO) 17:55 do something else
├─ Monday 28 July 2025
├─ Tuesday 29 July 2025
│ ├─ START: (DONE) 09:00 completed task                                :test:
│ └─ TODO (10:00) repeating task with last repeat                      :test:
├─ Wednesday 30 July 2025
│ └─ DEADLINE: TODO 10:00 send the professor a mail
└─ Thursday 31 July 2025
"
         "agenda options")
        "agenda options")))

(defun test-org-header-bounded-tree-func ()
  (let ((input "** TODO [#A] header text [50%] :tag1:tag2:"))
    (cltpt/combinator:parse
     input
     (rules-from-symbols '(cltpt/org-mode:org-header)))))

(test test-org-header-1
  (is-match-tree
   (car (test-org-header-bounded-tree-func))
   '((:BEGIN 0 :END 42 :ID CLTPT/ORG-MODE:ORG-HEADER)
     ((:BEGIN 0 :END 42)
      ((:BEGIN 0 :END 2 :ID CLTPT/ORG-MODE::STARS))
      ((:BEGIN 2 :END 3))
      ((:BEGIN 3 :END 8)
       ((:BEGIN 0 :END 4 :ID CLTPT/ORG-MODE::TODO-KEYWORD))
       ((:BEGIN 4 :END 5)))
      ((:BEGIN 8 :END 13 :ID CLTPT/ORG-MODE::PRIORITY)
       ((:BEGIN 0 :END 2))
       ((:BEGIN 2 :END 3))
       ((:BEGIN 3 :END 5)))
      ((:BEGIN 13 :END 24 :ID CLTPT/ORG-MODE::TITLE))
      ((:BEGIN 24 :END 30 :ID CLTPT/ORG-MODE::COMPLETION-STATUS)
       ((:BEGIN 0 :END 2))
       ((:BEGIN 2 :END 5))
       ((:BEGIN 5 :END 6)))
      ((:BEGIN 30 :END 42 :ID CLTPT/ORG-MODE::TAGS)
       ((:BEGIN 0 :END 2))
       ((:BEGIN 2 :END 11)
        ((:BEGIN 0 :END 4 :ID CLTPT/ORG-MODE::TAG))
        ((:BEGIN 4 :END 5))
        ((:BEGIN 5 :END 9 :ID CLTPT/ORG-MODE::TAG)))
       ((:BEGIN 11 :END 12)))))))

(test test-org-header-2
  (let* ((input "* head [[link:dest][desc]] tail [10%]")
         (rules (org-rules))
         (result (cltpt/combinator:parse input rules)))
    (fiveam:is
     (match-tree-equal-p
      (car result)
      '((:BEGIN 0 :END 37 :ID CLTPT/ORG-MODE:ORG-HEADER)
        ((:BEGIN 0 :END 37)
         ((:BEGIN 0 :END 1 :ID CLTPT/ORG-MODE::STARS))
         ((:BEGIN 1 :END 2))
         ((:BEGIN 2 :END 31 :ID CLTPT/ORG-MODE::TITLE)
          ((:BEGIN 5 :END 24 :ID CLTPT/ORG-MODE:ORG-LINK)
           ((:BEGIN 0 :END 19)
            ((:BEGIN 0 :END 2))
            ((:BEGIN 2 :END 6 :ID CLTPT/ORG-MODE::LINK-TYPE))
            ((:BEGIN 6 :END 7))
            ((:BEGIN 7 :END 11 :ID CLTPT/ORG-MODE::LINK-DEST))
            ((:BEGIN 11 :END 13))
            ((:BEGIN 13 :END 17 :ID CLTPT/ORG-MODE::LINK-DESC))
            ((:BEGIN 17 :END 19)))))
         ((:BEGIN 31 :END 37 :ID CLTPT/ORG-MODE::COMPLETION-STATUS)
          ((:BEGIN 0 :END 2))
          ((:BEGIN 2 :END 5))
          ((:BEGIN 5 :END 6)))))))))

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