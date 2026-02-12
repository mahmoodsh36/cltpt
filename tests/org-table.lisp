(defpackage :cltpt/tests/org-table
  (:use :cl :it.bese.fiveam)
  (:export :run-org-table-tests))

(in-package :cltpt/tests/org-table)

(def-suite org-table-suite
  :description "tests for org-table functionality."
  :in cltpt/tests::cltpt-suite)

(in-suite org-table-suite)

(defun test-coordinate-functions ()
  "an example demonstrating the use of get-cell-coordinates and
get-cell-at-coordinates."
  (let* ((table-string "| name      | age | occupation  |
|-----------+-----+-------------|
| alice     |  30 | engineer    |
| bob       |  25 | designer    |
| charlie   |  35 | programmer  |")
         (table-match (cltpt/org-mode::org-table-matcher nil (cltpt/reader:reader-from-string table-string) 0)))
    (format t "--- testing get-cell-at-coordinates ---~%")
    (let* ((coords (cons 1 2)) ; column 1, row 2 (0-indexed) -> "designer"
           (target-cell (cltpt/org-mode::get-cell-at-coordinates table-match coords)))
      (format t "requesting cell at coordinates: ~A~%" coords)
      (if target-cell
          (let* ((content-node (first (cltpt/combinator/match:match-children target-cell)))
                 (cell-text (cltpt/combinator:match-text content-node table-string)))
            (format t "found cell: ~A~%" target-cell)
            (format t "cell content: \"~A\"~%~%" cell-text)
            (format t "--- testing get-cell-coordinates ---~%")
            (let ((retrieved-coords (cltpt/org-mode::get-cell-coordinates target-cell)))
              (format t "coordinates of the found cell: ~A~%" retrieved-coords)
              (format t
                      "coordinates match original request: ~A~%"
                      (equal retrieved-coords coords))))
          (format t "cell not found at coordinates ~A~%" coords)))))

(defun test-reformat-and-get-cell ()
  "Demonstrates the correct workflow for working with a reformatted table."
  (let* ((original-table-string "| name    | age | occupation |
|---------+-----+------------|
| alice   | 30  | engineer   |
| bob     | 25  | designer   |")
         (original-reader (cltpt/reader:reader-from-string original-table-string))
         (coords (cons 1 2))) ;; we want bob's age
    (format t "--- analyzing original table ---~%")
    (multiple-value-bind (original-table-match end-pos)
        (cltpt/org-mode::org-table-matcher nil original-reader 0)
      (declare (ignore end-pos))
      (let* ((original-cell (cltpt/org-mode::get-cell-at-coordinates original-table-match coords))
             (cell-text (cltpt/combinator:match-text
                         (first (cltpt/combinator:match-children original-cell))
                         original-reader)))
        (format t "original table string:~%~a~%" original-table-string)
        (format t "cell at ~A has content: \"~A\"~%~%" coords cell-text))
      (format t "--- analyzing reformatted table ---~%")
      (let ((new-table-string (cltpt/org-mode::reformat-table original-reader original-table-match)))
        ;; re-parse the new string
        (let ((new-reader (cltpt/reader:reader-from-string new-table-string)))
          (multiple-value-bind (reformatted-table-match new-end-pos)
              (cltpt/org-mode::org-table-matcher nil new-reader 0)
            (let* ((reformatted-cell (cltpt/org-mode::get-cell-at-coordinates
                                      reformatted-table-match coords))
                   (cell-text (cltpt/combinator:match-text
                               (first (cltpt/combinator/match:match-children reformatted-cell))
                               new-reader)))
              (format t "reformatted table string:~%~a" new-table-string)
              (format t "cell at ~A now has content: \"~A\"~%" coords cell-text))))))))

(defun test-data-conversion-cycle ()
  "demonstrates the full cycle of parsing a table, converting to a
list, modifying the list, and converting back to a string."
  ;; --- 1. start with a string and parse it ---
  (let* ((table-string "| name    | species   |
|---------+-----------|
| frodo   | hobbit    |
| gandalf | maiar     |")
         (table-match (cltpt/org-mode::org-table-matcher nil (cltpt/reader:reader-from-string table-string) 0)))
    (format t "--- 1. original table string ---~%~A~%~%" table-string)
    ;; --- 2. convert match to nested list ---
    (let ((nested-data (cltpt/org-mode::table-match-to-nested-list table-match)))
      (format t "--- 2. converted to nested list ---~%")
      (format t "~S~%~%" nested-data)
      ;; --- 3. modify the data structure ---
      (format t "--- 3. modifying the list (adding a row) ---~%")
      (setf nested-data (append nested-data '(("aragorn" "human"))))
      (format t "new list: ~S~%~%" nested-data)
      ;; --- 4. convert the list back to a formatted string ---
      (let ((new-table-string (cltpt/org-mode::nested-list-to-table-string nested-data)))
        (format t "--- 4. converted back to string ---~%~A~%" new-table-string)
        ;; --- 5. parse the new string to get its match object ---
        (let ((new-table-match (cltpt/org-mode::org-table-matcher nil (cltpt/reader:reader-from-string new-table-string) 0)))
          (format t "--- 5. new string is parsable ---~%")
          (format t "resulting match object: ~A~%" new-table-match))))))

(defun test-hrule-inclusion ()
  "demonstrates the effect of the include-hrules-p flag."
  (let* ((table-string "| name  | role     |
|-------+----------|
| alice | leader   |
| bob   | follower |")
         (table-match (cltpt/org-mode::org-table-matcher nil (cltpt/reader:reader-from-string table-string) 0)))
    (let ((nested-data-with-hrules (cltpt/org-mode::table-match-to-nested-list table-match)))
      (format t "~S~%" nested-data-with-hrules))
    (let ((nested-data-without-hrules
            (cltpt/org-mode::table-match-to-nested-list table-match nil)))
      (format t "~S~%~%" nested-data-without-hrules)
      (format t "--- re-rendering the data without hrules ---~%")
      (let ((new-table-string (cltpt/org-mode::nested-list-to-table-string
                               nested-data-without-hrules)))
        (format t "the resulting table has no horizontal separators:~%~A" new-table-string)))))

(defun test-table-navigation ()
  "demonstrates the table dimension and navigation functions."
  (let* ((table-string "| A         | B         | C         |
|-----------+-----------+-----------|
| A1        | B1        | C1        |
| A2        | B2        | C2        |")
         (table-match (cltpt/org-mode::org-table-matcher nil (cltpt/reader:reader-from-string table-string) 0)))
    (let ((height (cltpt/org-mode::get-table-height table-match))
          (width (cltpt/org-mode::get-table-width table-match)))
      (format t "height: ~A~%" height)
      (format t "width : ~A~%" width))
    (format t "--- iterating through all data cells ---~%")
    (loop for coords = (cons 0 0) then (cltpt/org-mode::get-next-data-cell-coords table-match coords)
          while coords
          do
             (let* ((cell (cltpt/org-mode::get-cell-at-coordinates table-match coords))
                    (content-node (first (cltpt/combinator/match:match-children cell)))
                    (cell-text (if content-node
                                   (cltpt/combinator:match-text content-node table-string)
                                   "")))
               (format t "coords: ~A -> content: \"~A\"~%" coords cell-text)))))

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
      (format t "--- original partial table ---~%~A~%~%" table-string)
      (let ((reformatted-string (cltpt/org-mode::reformat-table reader table-match)))
        (format t "--- reformatted and completed table ---~%~A" reformatted-string)
        reformatted-string))))

(test test-reformat-partial-table
  (let ((result (test-reformat-partial-table))
        (expected-result "| head1 | head2 | head3 |
|-------+-------+-------|
| foo   | bar   | baz   |
|       |       |       |
| 123   | 1     |       |"
                         ))
    (fiveam:is (string= result
                        expected-result))))

;; Table parsing tests moved from org-mode.lisp

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

;; Additional table tests moved from org-mode.lisp

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

;; (defun test-parse-table-func ()
;;   (let ((table
;;           "| head1 | head2 | head3 |
;; +------+-------+-------+
;; | foo  | bar   | baz   |
;; | 123  | 456   | 789   |
;; +------+-------+-------+
;; | end  | row   | test  |"))
;;     (equal
;;      (org-table-parse table)
;;      '(("head1" "head2" "head3")
;;        ("foo" "bar" "baz")
;;        ("123" "456" "789")
;;        ("end" "row" "test")))))

;; (test test-parse-table
;;   (fiveam:is (test-parse-table-func)))

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
          ;; note: reformat-table might normalize whitespace/padding, but for this simple table
          ;; it should be fine.
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