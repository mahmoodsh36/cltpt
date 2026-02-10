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
             "\\ref{"
             (:pattern (cltpt/combinator:symbol-matcher) :id link-dest)
             "}")))
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

(defun transformer-test-3-func ()
  (let* ((str "[[id:myid][desc]]")
         (reader (cltpt/reader:reader-from-string str))
         (parsed (cltpt/combinator:parse
                  str
                  cltpt/org-mode::org-link))
         (dest-rule
           '(:pattern
             (cltpt/combinator:consec
              "\\ref{"
              (:pattern (cltpt/combinator:symbol-matcher)
               :id link-dest)
              "}")
             :id latex-link)))
    (cltpt/transform:reconstruct reader (car parsed) dest-rule)))

(defun transformer-test-4-func ()
  (let* ((full-string "[[attachment:sliding]]")
         (reader (cltpt/reader:reader-from-string full-string))
         (parsed (cltpt/combinator:parse
                  full-string
                  '((:pattern
                     (cltpt/combinator:consec
                      "[["
                      (:pattern (cltpt/combinator:symbol-matcher) :id link-type)
                      ":"
                      (:pattern (cltpt/combinator:all-but "[]") :id link-dest)
                      "]]")
                     :id link)))))
    (cltpt/transform:transform
     reader
     (car parsed)
     '((link . (:pattern (cltpt/combinator:separated-atleast-one ",") :id link-dest))))))

(test transformer-test-4
  (fiveam:is
   (string= (transformer-test-4-func)
            "[[,attachment,:,sliding,]]")))

(defun run-transform-tests ()
  (format t "~&running transform tests...~%")
  (let ((results (run! 'transform-suite)))
    (unless results
      (explain! results))))