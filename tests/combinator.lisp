(defpackage :cltpt/tests/combinator
  (:use :cl :it.bese.fiveam)
  (:import-from
   :cltpt/tests
   :plist-to-match
   :compare-full-match-loosely)
  (:import-from
   :cltpt/tests/utils
   :org-rules
   :rules-from-symbols)
  (:export :run-combinator-tests))

(in-package :cltpt/tests/combinator)

;; this is for verifying the parser-combinator works properly. the higher level parsing is tested elsewhere

(def-suite combinator-suite
  :description "tests for the combinator parsing primitives."
  :in cltpt/tests::cltpt-suite)

(in-suite combinator-suite)

(defun test-combinator-number-1 ()
  (cltpt/combinator:apply-rule
   nil
   '(cltpt/combinator:natural-number-matcher)
   (cltpt/reader:reader-from-string "2023")
   0))

(test test-combinator-number-1
  (let ((result (test-combinator-number-1)))
    (fiveam:is (= (cltpt/combinator/match::match-end result) 4))))

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
       '((:ID ORG-LINK :BEGIN 0 :END 18)
         ((:BEGIN 0 :END 2))
         ((:ID TEST2 :BEGIN 2 :END 7))
         ((:BEGIN 7 :END 8))
         ((:ID TEST3 :BEGIN 8 :END 11))
         ((:BEGIN 11 :END 13))
         ((:BEGIN 13 :END 16))
         ((:BEGIN 16 :END 18))))))

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
         ((:BEGIN 0 :END 2)) ((:BEGIN 2 :END 8)))
        ((:ID ENDING :BEGIN 35 :END 36)))))))

(defun test-pairs-2-func ()
  (let* ((other-rules `((:pattern ,(cltpt/combinator:handle-rule-string "#+%w")
                         :id keyword)))
         (rules
           `((cltpt/combinator::pair
              (:pattern
               (cltpt/combinator::unescaped (cltpt/combinator::literal "*"))
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
                  ((:ID OPENINGG :BEGIN 0 :END 1))
                  ((:ID KEYWORD :BEGIN 10 :END 17))
                  ((:ID ENDINGG :BEGIN 16 :END 17)))))))

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
                  ((:ID OPENINGG :BEGIN 0 :END 1))
                  ((:ID KEYWORD :BEGIN 10 :END 17))
                  ((:ID ENDINGG :BEGIN 16 :END 17)))))))

(defun test-sharp-lisp-1-func ()
  (let* ((rules
           '((:pattern
              (cltpt/combinator::consec
               (cltpt/combinator::literal "#")
               (:pattern (cltpt/combinator::lisp-sexp)
                :id lisp-code))
              :id sharp-lisp-block)))
         (input-string "#(format t \"hello)(\\\" there\") "))
    (cltpt/combinator::parse input-string rules)))

(test test-sharp-lisp-1
  (fiveam:is
   (compare-full-match-loosely
    (car (test-sharp-lisp-1-func))
    '((:ID SHARP-LISP-BLOCK :BEGIN 0 :END 29)
      ((:BEGIN 0 :END 1))
      ((:ID LISP-CODE :BEGIN 1 :END 29))))))

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
                '((:ID ORG-LINK :BEGIN 32 :END 50)
                  ((:BEGIN 0 :END 2))
                  ((:ID TEST2 :BEGIN 2 :END 7))
                  ((:BEGIN 7 :END 8))
                  ((:ID TEST3 :BEGIN 8 :END 11))
                  ((:BEGIN 11 :END 13))
                  ((:BEGIN 13 :END 16))
                  ((:BEGIN 16 :END 18)))))
    (fiveam:is (compare-full-match-loosely
                (cadr result)
                '((:ID ORG-LINK :BEGIN 72 :END 90)
                  ((:BEGIN 0 :END 2))
                  ((:ID TEST2 :BEGIN 2 :END 5))
                  ((:BEGIN 5 :END 6))
                  ((:ID TEST3 :BEGIN 6 :END 9))
                  ((:BEGIN 9 :END 11))
                  ((:BEGIN 11 :END 15))
                  ((:BEGIN 15 :END 17)))))))

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
                  ((:BEGIN 0 :END 1))
                  ((:BEGIN 1 :END 5)))))))

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
                  ((:BEGIN 0 :END 1))
                  ((:BEGIN 1 :END 5)))))))

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
        ((:BEGIN 0 :END 1))
        ((:BEGIN 1 :END 2))
        ((:ID TAG :BEGIN 2 :END 13)
         ((:BEGIN 0 :END 5 :ID TAG))
         ((:BEGIN 6 :END 11 :ID TAG)))
        ((:BEGIN 13 :END 14)))))))

(defun test-flanked-by-whitespace-or-punctuation-func ()
  (let ((test-rule `(:pattern (cltpt/combinator:flanked-by-whitespace-or-punctuation
                               (:pattern (cltpt/combinator:literal "test")
                                :id test-word)))))
    (cltpt/combinator:parse " hello test world" (list test-rule))))

(test test-flanked-by-whitespace-or-punctuation
  (fiveam:is
   (compare-full-match-loosely
    (car (test-flanked-by-whitespace-or-punctuation-func))
    '((:BEGIN 6 :END 10 :MATCH "test")))))

(defun test-flanked-by-whitespace-or-punctuation-punctuation-func ()
  (let ((test-rule `(:pattern (cltpt/combinator:flanked-by-whitespace-or-punctuation
                               (:pattern (cltpt/combinator:literal "test")
                                :id test-word)))))
    (cltpt/combinator:parse "hello,test,world" (list test-rule))))

(test test-flanked-by-whitespace-or-punctuation-punctuation
  (fiveam:is
   (compare-full-match-loosely
    (car (test-flanked-by-whitespace-or-punctuation-punctuation-func))
    '((:BEGIN 6 :END 10 :MATCH "test")))))

(defun test-flanked-by-whitespace-or-punctuation-no-match-func ()
  (let ((test-rule `(:pattern (cltpt/combinator:flanked-by-whitespace-or-punctuation
                               (:pattern (cltpt/combinator:literal "test")
                                :id test-word)))))
    (cltpt/combinator:parse "atestb" (list test-rule))))

(test test-flanked-by-whitespace-or-punctuation-no-match
  (fiveam:is
   (null (test-flanked-by-whitespace-or-punctuation-no-match-func))))

;; a following few tests test some stuff on my personal machine but ill keep them here for now
(defun org-combinator-test-1 ()
  (time
   (let ((files (uiop:directory-files "/Volumes/main/brain/notes/"
                                      "*.org")))
     (loop for file in files
           for i from 0
           for text = (cltpt/file-utils:read-file file)
           do (let ((tree (cltpt/combinator:parse
                           text
                           (org-rules))))
                ;; (format t "done ~A ~A~%" i file)
                )))))

(defun org-combinator-test-2 ()
  (time
   (let ((files (uiop:directory-files "/Volumes/main/brain/notes/"
                                      "*.org")))
     (loop for file in files
           for i from 0
           for text = (cltpt/file-utils:read-file file)
           do (let ((tree (cltpt/combinator:parse
                           text
                           (rules-from-symbols
                            '(cltpt/org-mode::org-link
                              cltpt/org-mode::org-timestamp
                              )))))
                ;; (format t "done ~A ~A~%" i file)
                )))))

(defun org-combinator-test-3 ()
  (time
   (let ((files (uiop:directory-files "/Volumes/main/brain/notes/"
                                      "*.org")))
     (loop for file in files
           ;; for i from 0
           do (with-open-file (stream file)
                (let ((tree (cltpt/combinator:parse
                             stream
                             (org-rules))))
                  ;; (format t "done ~A ~A~%" i file)
                  ))))))

(defun match-tree-equal-p (actual expected)
  "recursively compare an actual match tree against an expected plist tree.
EXPECTED format: ((:ID id :BEGIN begin :END end) child1 child2 ...)
where each child has the same format.
returns T if trees match, NIL otherwise."
  (when (and (null actual) (null expected))
    (return-from match-tree-equal-p t))
  (when (or (null actual) (null expected))
    (return-from match-tree-equal-p nil))
  (let* ((expected-info (car expected))
         (expected-children (cdr expected))
         (expected-id (getf expected-info :id))
         (expected-begin (getf expected-info :begin))
         (expected-end (getf expected-info :end)))
    ;; check current node
    (unless (and (eql (cltpt/combinator/match:match-begin actual) expected-begin)
                 (eql (cltpt/combinator/match:match-end actual) expected-end)
                 (eql (cltpt/combinator/match:match-id actual) expected-id))
      (return-from match-tree-equal-p nil))
    ;; check children count matches
    (unless (= (length (cltpt/combinator/match:match-children actual))
               (length expected-children))
      (return-from match-tree-equal-p nil))
    ;; recursively check all children
    (loop for actual-child in (cltpt/combinator/match:match-children actual)
          for expected-child in expected-children
          always (match-tree-equal-p actual-child expected-child))))

(defun describe-tree-mismatch (actual expected &optional (depth 0))
  "return a string describing where the trees differ."
  (let ((indent (make-string (* depth 2) :initial-element #\space)))
    (when (and (null actual) (null expected))
      (return-from describe-tree-mismatch nil))
    (when (null actual)
      (return-from describe-tree-mismatch
        (format nil "~Aexpected node but got NIL: ~S" indent expected)))
    (when (null expected)
      (return-from describe-tree-mismatch
        (format nil "~Aunexpected node: ~A" indent actual)))
    (let* ((expected-info (car expected))
           (expected-children (cdr expected))
           (expected-id (getf expected-info :id))
           (expected-begin (getf expected-info :begin))
           (expected-end (getf expected-info :end))
           (actual-id (cltpt/combinator/match:match-id actual))
           (actual-begin (cltpt/combinator/match:match-begin actual))
           (actual-end (cltpt/combinator/match:match-end actual))
           (actual-children (cltpt/combinator/match:match-children actual)))
      (unless (eql actual-begin expected-begin)
        (return-from describe-tree-mismatch
          (format nil "~ABEGIN mismatch: expected ~A, got ~A (id: ~A)"
                  indent expected-begin actual-begin expected-id)))
      (unless (eql actual-end expected-end)
        (return-from describe-tree-mismatch
          (format nil "~AEND mismatch: expected ~A, got ~A (id: ~A)"
                  indent expected-end actual-end expected-id)))
      (unless (eql actual-id expected-id)
        (return-from describe-tree-mismatch
          (format nil "~AID mismatch: expected ~A, got ~A"
                  indent expected-id actual-id)))
      (unless (= (length actual-children) (length expected-children))
        (return-from describe-tree-mismatch
          (format nil "~Achildren count mismatch: expected ~A, got ~A (id: ~A)"
                  indent (length expected-children) (length actual-children) actual-id)))
      (loop for actual-child in actual-children
            for expected-child in expected-children
            for result = (describe-tree-mismatch actual-child expected-child (1+ depth))
            when result return result))))

(defun match-trees-equal-p (actual-list expected-list)
  "compare a list of match trees against a list of expected plist trees.
returns T if all trees match, NIL otherwise."
  (and (= (length actual-list) (length expected-list))
       (loop for actual in actual-list
             for expected in expected-list
             always (match-tree-equal-p actual expected))))

(defun describe-trees-mismatch (actual-list expected-list)
  "return a string describing where the tree lists differ."
  (unless (= (length actual-list) (length expected-list))
    (return-from describe-trees-mismatch
      (format nil "count mismatch: expected ~A matches, got ~A"
              (length expected-list) (length actual-list))))
  (loop for actual in actual-list
        for expected in expected-list
        for i from 0
        for mismatch = (describe-tree-mismatch actual expected)
        when mismatch
          return (format nil "match ~A: ~A" i mismatch)))

(defun test-org-full-tree-structure-func ()
  (let ((input "* header
some text with a [[link:path][description]].

#+begin_src python
print(\"hello\")
#+end_src
"))
    (cltpt/combinator:parse input (org-rules))))

(test test-org-full-tree-structure
  (let* ((result (test-org-full-tree-structure-func))
         (expected-tree
           '(((:id cltpt/org-mode:org-header :begin 0 :end 8)
              ((:id nil :begin 0 :end 8)
               ((:id nil :begin 0 :end 8)
                ((:id cltpt/org-mode::stars :begin 0 :end 1))
                ((:id nil :begin 1 :end 2))
                ((:id cltpt/org-mode::title :begin 2 :end 8)))))
             ((:id cltpt/org-mode:org-link :begin 26 :end 52)
              ((:id nil :begin 0 :end 26)
               ((:id nil :begin 0 :end 2))
               ((:id cltpt/org-mode::link-type :begin 2 :end 6))
               ((:id nil :begin 6 :end 7))
               ((:id cltpt/org-mode::link-dest :begin 7 :end 11))
               ((:id nil :begin 11 :end 13))
               ((:id cltpt/org-mode::link-desc :begin 13 :end 24))
               ((:id nil :begin 24 :end 26))))
             ((:id cltpt/org-mode:org-src-block :begin 55 :end 98)
              ((:id nil :begin 0 :end 43)
               ((:id cltpt/org-mode::begin :begin 0 :end 18)
                ((:id nil :begin 0 :end 18)
                 ((:id cltpt/org-mode::open-tag :begin 0 :end 11))
                 ((:id nil :begin 11 :end 12))
                 ((:id cltpt/org-mode::lang :begin 12 :end 18))))
               ((:id cltpt/org-mode::end :begin 34 :end 43)))))))
    (fiveam:is (match-trees-equal-p result expected-tree)
               (format nil "tree mismatch: ~A"
                       (describe-trees-mismatch result expected-tree)))))

(defun test-simple-rules-full-tree-func ()
  (let* ((hashtag-rule
           `(:pattern
             (cltpt/combinator::consec
              (:pattern (cltpt/combinator::literal "#") :id hash)
              (:pattern (cltpt/combinator::word-digits-hyphen) :id tag-name))
             :id hashtag))
         (keyval-rule
           `(:pattern
             (cltpt/combinator::consec
              (:pattern (cltpt/combinator::word-digits-hyphen) :id key)
              (:pattern (cltpt/combinator::literal ":") :id colon)
              (:pattern (cltpt/combinator::word-digits-hyphen) :id value))
             :id key-value))
         (rules (list hashtag-rule keyval-rule))
         (input "some text #tag1 and key:value here #tag2"))
    (cltpt/combinator:parse input rules)))

(test test-simple-rules-full-tree
  (let* ((result (test-simple-rules-full-tree-func))
         (expected-tree
           '(((:id hashtag :begin 10 :end 15)
              ((:id hash :begin 0 :end 1))
              ((:id tag-name :begin 1 :end 5)))
             ((:id key-value :begin 20 :end 29)
              ((:id key :begin 0 :end 3))
              ((:id colon :begin 3 :end 4))
              ((:id value :begin 4 :end 9)))
             ((:id hashtag :begin 35 :end 40)
              ((:id hash :begin 0 :end 1))
              ((:id tag-name :begin 1 :end 5))))))
    (fiveam:is (match-trees-equal-p result expected-tree)
               (format nil "tree mismatch: ~A"
                       (describe-trees-mismatch result expected-tree)))))

(defun run-combinator-tests ()
  (format t "~&running combinator tests...~%")
  (let ((results (run! 'combinator-suite)))
    (unless results
      (explain! results))))