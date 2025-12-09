(defpackage :cltpt/tests/combinator
  (:use :cl :it.bese.fiveam)
  (:import-from :cltpt/tests
                :plist-to-match
                :compare-full-match-loosely)
  (:export #:run-combinator-tests))

(in-package :cltpt/tests/combinator)

(def-suite combinator-suite
  :description "tests for the combinator parsing primitives."
  :in cltpt/tests::cltpt-suite)

(in-suite combinator-suite)

(defun test-combinator-number-1 ()
  (cltpt/combinator:apply-rule-normalized
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
         ((:BEGIN 25 :END 27)) ((:BEGIN 27 :END 33)))
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
                  ((:ID OPENINGG :BEGIN 2 :END 3))
                  ((:ID KEYWORD :BEGIN 12 :END 19))
                  ((:ID ENDINGG :BEGIN 18 :END 19)))))))

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
                  ((:ID KEYWORD :BEGIN 12 :END 19))
                  ((:ID ENDINGG :BEGIN 18 :END 19)))))))

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
                  ((:BEGIN 32 :END 34))
                  ((:ID TEST2 :BEGIN 34 :END 39))
                  ((:BEGIN 39 :END 40))
                  ((:ID TEST3 :BEGIN 40 :END 43))
                  ((:BEGIN 43 :END 45))
                  ((:BEGIN 45 :END 48))
                  ((:BEGIN 48 :END 50)))))
    (fiveam:is (compare-full-match-loosely
                (cadr result)
                '((:ID ORG-LINK :BEGIN 72 :END 90)
                  ((:BEGIN 72 :END 74))
                  ((:ID TEST2 :BEGIN 74 :END 77))
                  ((:BEGIN 77 :END 78))
                  ((:ID TEST3 :BEGIN 78 :END 81))
                  ((:BEGIN 81 :END 83))
                  ((:BEGIN 83 :END 87))
                  ((:BEGIN 87 :END 89)))))))

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

;; test-separated-atleast-one and test-flanked-by-whitespace-or-punctuation tests

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

(defun run-combinator-tests ()
  (format t "~&running combinator tests...~%")
  (let ((results (run! 'combinator-suite)))
    (unless results
      (explain! results))))