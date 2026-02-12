(defpackage :cltpt/tests/org-table
  (:use :cl :it.bese.fiveam)
  (:export :run-org-table-tests))

(in-package :cltpt/tests/org-table)

(def-suite org-table-suite
  :description "tests for org-table functionality."
  :in cltpt/tests::cltpt-suite)

(in-suite org-table-suite)

(test test-coordinate-functions
  "tests that get-cell-coordinates and get-cell-at-coordinates work correctly together."
  (let* ((table-string "| name      | age | occupation  |
|-----------+-----+-------------|
| alice     |  30 | engineer    |
| bob       |  25 | designer    |
| charlie   |  35 | programmer  |")
         (table-match (cltpt/org-mode::org-table-matcher
                       nil
                       (cltpt/reader:reader-from-string table-string)
                       0))
         (test-cells '(((0 . 0) . "name")
                       ((1 . 0) . "age")
                       ((2 . 0) . "occupation")
                       ((0 . 2) . "bob")
                       ((1 . 2) . "25")
                       ((2 . 2) . "designer")
                       ((0 . 3) . "charlie")
                       ((1 . 3) . "35")
                       ((2 . 3) . "programmer")))
         (all-passed t)
         (errors))
    (dolist (test-cell test-cells)
      (let* ((coords (car test-cell))
             (expected-content (cdr test-cell))
             (target-cell (cltpt/org-mode::get-cell-at-coordinates table-match coords)))
        (cond
          ((null target-cell)
           (push (format nil "cell not found at coordinates ~A" coords) errors)
           (setf all-passed nil))
          (t
           (let* ((content-node (first (cltpt/combinator/match:match-children target-cell)))
                  (actual-content (cltpt/combinator:match-text content-node table-string))
                  (retrieved-coords (cltpt/org-mode::get-cell-coordinates target-cell)))
             (unless (equal retrieved-coords coords)
               (push (format nil
                             "retrieved coordinates ~A do not match original ~A"
                             retrieved-coords coords)
                     errors)
               (setf all-passed nil))
             (unless (string= actual-content expected-content)
               (push (format nil
                             "cell at ~A contains '~A', expected '~A'"
                             coords actual-content expected-content)
                     errors)
               (setf all-passed nil)))))))
    (is-true all-passed (format nil "coordinate test failures:~%~{  ~A~%~}" (nreverse errors)))))

(defun test-reformat-partial-table ()
  (let* ((table-string "| head1 | head2 | head3 |
|-------+-------+-------|
| foo   | bar   | baz   |
|||
| 123   | 1
hi")
         (reader (cltpt/reader:reader-from-string table-string))
         (table-match (cltpt/org-mode::org-table-matcher nil reader 0)))
    (when table-match
      (let ((reformatted-string (cltpt/org-mode::reformat-table reader table-match)))
        reformatted-string))))

(test test-reformat-partial-table
  (let ((result (test-reformat-partial-table))
        (expected-result "| head1 | head2 | head3 |
|-------+-------+-------|
| foo   | bar   | baz   |
|       |       |       |
| 123   | 1     |       |"
                         ))
    (fiveam:is (string= result expected-result))))

(defun test-org-table-1 ()
  (let* ((text
           "| head1 | head2 | head3 |
+------+-------+-------+
| foo | \\(mymath\\) | baz  |
| 123 | 456          | 789  |
|     |              | 1     |
| end | row          | test |
some more text")
         (reader (cltpt/reader:reader-from-string text)))
    (cltpt/org-mode::org-table-matcher
     nil
     reader
     0
     '((:pattern (cltpt/combinator::pair
                  (cltpt/combinator::literal "\\(")
                  (cltpt/combinator::literal "\\)")
                  nil)
        :id mypair)))))

(defun test-org-table-1-func ()
  (let* ((text
           "| head1 | head2 | head3 |
+------+-------+-------+
| foo | \\(mymath\\) | baz  |
| 123 | 456          | 789  |
|     |              | 1     |
| end | row          | test |
some more text")
         (reader (cltpt/reader:reader-from-string text)))
    (cltpt/org-mode::org-table-matcher
     nil
     reader
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
         (reader (cltpt/reader:reader-from-string misaligned-table-text))
         (parse-tree (cltpt/org-mode::org-table-matcher nil reader 0)))
    (when parse-tree
      (let ((formatted-table-text (cltpt/org-mode::reformat-table reader parse-tree)))
        formatted-table-text))))

(defun test-org-table-2-func ()
  (let* ((misaligned-table-text
           "| name | age|
|------+----|
|alice|  25 |
|  bob |30 |
|      |    |
| charlie| 9 |")
         (reader (cltpt/reader:reader-from-string misaligned-table-text))
         (parse-tree (cltpt/org-mode::org-table-matcher nil reader 0)))
    (when parse-tree
      (cltpt/org-mode::reformat-table reader parse-tree))))

(test test-org-table-2
  (let ((result (test-org-table-2-func)))
    (fiveam:is (stringp result))
    (fiveam:is (search "name" result))
    (fiveam:is (search "alice" result))
    (fiveam:is (search "charlie" result))))

(defun test-parse-table-func-2 ()
  (let ((table "| head1 | head2 | head3 |
+------+-------+-------+
| foo  | bar   | baz   |
| 123  | 456   | 789   |
+------+-------+-------+
| end  | row   | test  |"
               ))
    (cltpt/org-mode::org-table-matcher nil (cltpt/reader:reader-from-string table) 0)))

(defun test-parse-table-custom-delimiters ()
  "tests the table parser with custom delimiters (#, =, and *)."
  (let ((cltpt/org-mode::*table-v-delimiter* #\#)
        (cltpt/org-mode::*table-h-delimiter* #\=)
        (cltpt/org-mode::*table-intersection-delimiter* #\*)
        (table "# header A # header B # header C #
#==========*==========*==========#
#   row 1  # val 1    #  abc     #
#   row 2  # val 2    #  def     #"))
    (cltpt/org-mode::org-table-matcher nil (cltpt/reader:reader-from-string table) 0)))

(defun test-parse-table-all-hash-delimiters ()
  "tests the table parser where all delimiter types are the '#' character."
  (let ((cltpt/org-mode::*table-v-delimiter* #\#)
        (cltpt/org-mode::*table-h-delimiter* #\#)
        (cltpt/org-mode::*table-intersection-delimiter* #\#)
        (table "# Column 1 # Column 2 #
# ######## # ######## #
#   Data A #   Data B #
#   Data C #   Data D #"))
    (cltpt/org-mode::org-table-matcher nil (cltpt/reader:reader-from-string table) 0)))

(defun test-reformat-table-func ()
  "tests the reformatting functionality by parsing a misaligned table
and then running reformat-table on the resulting parse tree."
  (let* ((disoriented-table
"|Name|Profession|Country|
|---+---|---|
|Ada Lovelace|Mathematician|England|
|Grace Hopper|Computer Scientist|USA|
|Alan Turing|Mathematician|England|
")
         (reader (cltpt/reader:reader-from-string disoriented-table))
         (parse-tree (cltpt/org-mode::org-table-matcher nil reader 0)))
    (when parse-tree
      (cltpt/org-mode::reformat-table reader parse-tree))))

;; this was added because of 0ed6b5bbe1f52c4a21167d1bf5e888a853d2c656
(test test-reformat-substring-relative-pos
  "ensures reformat-table works correctly when passed a substring (e.g. just the table text) but
the match object has absolute positions relative to a larger parent document."
  (let* ((prefix (format nil "some introductory text.~%~%"))
         (table-text "| col1 | col2 |
|------+------|
| val1 | val2 |"
                     )
         (full-text (concatenate 'string prefix table-text))
         (reader (cltpt/reader:reader-from-string full-text))
         (table-start-pos (position #\| full-text)))
    (is (not (null table-start-pos)) "could not find table start")
    (multiple-value-bind (table-match end-pos)
        (cltpt/org-mode::org-table-matcher nil reader table-start-pos)
      (is (not (null table-match)) "table match should be found")
      (when table-match
        ;; call reformat-table with just the table substring but the original match object (which
        ;; has absolute positions relative to full-text).
        ;; if absolute positions are used, this will crash or produce garbage/errors.
        ;; if relative positions are used, it should work fine.
        (let ((reformatted (cltpt/org-mode::reformat-table table-text table-match)))
          (is (search "col1" reformatted) "reformatted table should contain content"))
        (let ((data (cltpt/org-mode::table-match-to-nested-list table-text table-match)))
          (is (equal (first data) '("col1" "col2")) "header row should be correct")
          (is (equal (second data) :hrule) "should contain hrule")
          (is (equal (third data) '("val1" "val2")) "data row should be correct"))))))

(defun run-org-table-tests ()
  "run all org-table tests."
  (format t "~&running org-table tests...~%")
  (let ((results (run! 'org-table-suite)))
    (unless results
      (explain! results))))