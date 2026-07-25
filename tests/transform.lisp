(defpackage :cltpt/tests/transform
  (:use :cl :it.bese.fiveam)
  (:import-from
   :cltpt/tests
   :plist-to-match)
  (:export
   :run-transform-tests))

(in-package :cltpt/tests/transform)

(def-suite transform-suite
  :description "tests for the transform/reconstruct functionality."
  :in cltpt/tests::cltpt-suite)

(in-suite transform-suite)

(defun transformer-test-1-func ()
  (let* ((full-string "[[mylink1-2:here1][testmore1- 2]]")
         (reader (cltpt/reader:reader-from-string full-string))
         (parsed-plist
           `((:id org-link :begin 0 :end 33 :str ,full-string)
             ((:begin 0 :end 2 :str ,full-string))
             ((:id link-type :begin 2 :end 11 :str ,full-string))
             ((:begin 11 :end 12 :str ,full-string))
             ((:id link-dest :begin 12 :end 17 :str ,full-string))
             ((:begin 17 :end 19 :str ,full-string))
             ((:id link-desc :begin 19 :end 31 :str ,full-string))
             ((:begin 31 :end 33 :str ,full-string))))
         (parsed (plist-to-match parsed-plist))
         (dest-rule
           '(cltpt/combinator:consec
             "" ;; [[
             "" ;; link-type
             "\\ref{" ;; :
             (:pattern (cltpt/combinator:symbol-matcher) :id link-dest)
             "" ;; ][
             "" ;; link-desc
             "}" ;; ]]
             )))
    (cltpt/transform:reconstruct reader parsed dest-rule)))

(defun transformer-test-2-func ()
  (let* ((full-string "[[attachment:sliding]]")
         (reader (cltpt/reader:reader-from-string full-string))
         (parsed-plist
           `((:id org-link :begin 0 :end 22 :str ,full-string)
             ((:begin 0 :end 2 :str ,full-string))
             ((:id link-dest :begin 2 :end 20 :str ,full-string))
             ((:begin 20 :end 22 :str ,full-string))))
         (parsed (plist-to-match parsed-plist))
         (dest-rule
           '(:pattern (cltpt/combinator:consec
                       "\\ref{"
                       (:pattern (cltpt/combinator:symbol-matcher)
                        :id link-dest)
                       "}")
             :id latex-link)))
    (cltpt/transform:reconstruct reader parsed dest-rule)))

(test transformer-test-2
  (fiveam:is
   (string= (transformer-test-2-func)
            "\\ref{attachment:sliding}")))

(test transformer-test-1
  (fiveam:is
   (string= (transformer-test-1-func)
            "\\ref{here1}")))

(defun transformer-test-5-func ()
  (let* ((str "(([[test]]))")
         (reader (cltpt/reader:reader-from-string str))
         (src-rule
           '(cltpt/combinator:pair
             "("
             ")"
             :rules-for-content ((cltpt/combinator:consec
                                  "[["
                                  (:pattern (cltpt/combinator:all-but "[]") :id link-dest)
                                  "]]"))
             :nest-self t))
         (dest-rule
           '(cltpt/combinator:pair
             "{{ "
             " }}"
             :rules-for-content ((cltpt/combinator:consec
                                  "\\ref{"
                                  (:pattern (cltpt/combinator:all-but "[]") :id link-dest)
                                  "}"))))
         (parsed (cltpt/combinator:parse
                  str
                  `(,src-rule))))
    (cltpt/transform:reconstruct reader (car parsed) dest-rule)))

(test transformer-test-5
  (fiveam:is
   (string= (transformer-test-5-func)
            "{{ {{ \\ref{test} }} }}")))

(test transform-generate-number
  (fiveam:is (string= (cltpt/transform:generate '(cltpt/combinator:number-matcher) 42)
                      "42"))
  (fiveam:is (string= (cltpt/transform:generate '(cltpt/combinator:number-matcher) -3.5)
                      "-3.5")))

(defvar *transform-test-list-rule*
  '(cltpt/combinator:any
    (:pattern "[]" :value nil)
    (:pattern
     (cltpt/combinator:consec
      "["
      (cltpt/combinator:separated-atleast-one
       ", "
       *transform-test-value-rule*)
      "]")
     :get list)))
(defvar *transform-test-value-rule*
  '(cltpt/combinator:any
    *transform-test-list-rule*
    (cltpt/combinator:number-matcher)))

(test transform-generate-recursive-nesting
  (fiveam:is (string= (cltpt/transform:generate '*transform-test-value-rule* nil)
                      "[]"))
  (fiveam:is (string= (cltpt/transform:generate '*transform-test-value-rule* '(1 2 3))
                      "[1, 2, 3]"))
  (fiveam:is (string= (cltpt/transform:generate '*transform-test-value-rule* '(1 (2 (3 4)) nil))
                      "[1, [2, [3, 4]], []]")))

(test transform-babel-python-encode
  (fiveam:is
   (string= (cltpt/babel:babel-encode 'cltpt/babel::python 42)
            "42"))
  (fiveam:is
   (string= (cltpt/babel:babel-encode 'cltpt/babel::python "hey")
            "'hey'"))
  (fiveam:is
   (string= (cltpt/babel:babel-encode 'cltpt/babel::python nil)
            "[]"))
  (fiveam:is
   (string= (cltpt/babel:babel-encode 'cltpt/babel::python '(1 (2 "x" (4 5)) nil 3.5))
            "[1, [2, 'x', [4, 5]], [], 3.5]")))

(test transform-babel-python-decode
  (fiveam:is (eql (cltpt/babel:babel-decode 'cltpt/babel::python "42") 42))
  (fiveam:is (string= (cltpt/babel:babel-decode 'cltpt/babel::python "'hey'") "hey"))
  (fiveam:is (null (cltpt/babel:babel-decode 'cltpt/babel::python "[]")))
  (fiveam:is (equal (cltpt/babel:babel-decode 'cltpt/babel::python "[1, 2, 3]")
                    '(1 2 3)))
  (fiveam:is (equal (cltpt/babel:babel-decode 'cltpt/babel::python "[1, [2, 'x', [4, 5]], [], 3.5]")
                    '(1 (2 "x" (4 5)) nil 3.5))))

(defvar *transform-test-literal-consec-rule*
  '(cltpt/combinator:consec
    (cltpt/combinator:literal "<")
    (cltpt/combinator:number-matcher)
    (cltpt/combinator:literal ">")))

(test transform-decode-skips-literal-parts
  (let* ((text "<42>")
         (reader (cltpt/reader:reader-from-string text))
         (match (car (cltpt/combinator:parse
                      reader
                      (list '*transform-test-literal-consec-rule*)))))
    (fiveam:is (eql 42
                    (cltpt/transform:decode
                     reader
                     match
                     '*transform-test-literal-consec-rule*)))))

(test transform-decode-number
  (fiveam:is (eql 42 (cltpt/babel:babel-decode 'cltpt/babel::python "42")))
  (fiveam:is (eql -3 (cltpt/babel:babel-decode 'cltpt/babel::python "-3")))
  (fiveam:is (= 0.125 (cltpt/babel:babel-decode 'cltpt/babel::python "0.125")))
  (fiveam:is (= -3.5 (cltpt/babel:babel-decode 'cltpt/babel::python "-3.5"))))

;; decode is the inverse of encode: encoding a value then decoding the text recovers it.
(test transform-babel-python-roundtrip
  (dolist (value '(42 "hey" nil (1 2 3) (1 (2 "x" (4 5)) nil)))
    (fiveam:is (equal value
                      (cltpt/babel:babel-decode
                       'cltpt/babel::python
                       (cltpt/babel:babel-encode 'cltpt/babel::python value))))))

;; :get lets a `consec' target a non-list value, e.g. a CONS cell: it spreads the cons into
;; the list `generate-consec' distributes across the parts.
(defun transform-test-cons-to-list (c)
  (list (car c) (cdr c)))

(defvar *transform-test-pair-rule*
  '(:pattern
    (cltpt/combinator:consec
     (cltpt/combinator:number-matcher)
     ","
     (cltpt/combinator:number-matcher))
    :get transform-test-cons-to-list))

(test transform-generate-get-slices-cons
  (fiveam:is (string= (cltpt/transform:generate '*transform-test-pair-rule* (cons 3 4))
                      "3,4")))

(defun run-transform-tests ()
  (format t "~&running transform tests...~%")
  (let ((results (run! 'transform-suite)))
    (unless results
      (explain! results))))