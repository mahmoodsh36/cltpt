(defpackage :cltpt/tests
  (:use :cl :it.bese.fiveam :cltpt/base)
  (:export #:run-cltpt-tests))

(in-package :cltpt/tests)

(def-suite cltpt-suite
  :description "tests for cltpt.")

(in-suite cltpt-suite)

(defun test-convert-1 ()
  (let ((parsed
          (cltpt/base:parse
           cltpt/org-mode:*org-mode*
           "#(cltpt/base::make-block :type 'theorem :let '((a \"some text\")))
  my first block
  %a
  %(identity a)
  #(cltpt/base::make-block :type 'subtheorem :let '((b \" that will be included on export\")))
    hello
    %(concatenate 'string a b)
  #(cltpt/base::block-end)
#(cltpt/base::block-end)")))
    (cltpt/tree:tree-show parsed)
    (cltpt/base:convert-tree parsed cltpt/org-mode:*org-mode* cltpt/html:*html*)))

(test test-convert-1
  (let ((result (test-convert-1)))
    (fiveam:is (stringp result))
    (fiveam:is (> (length result) 0))))

(defun test-convert-2 ()
  (let ((parsed
          (cltpt/base:parse
           cltpt/org-mode:*org-mode*
           "#(cltpt/base::make-block :type 'theorem :let '((a \"some text\")))
  my block ~here~
  %a
#(cltpt/base::block-end)")))
    (cltpt/tree:tree-show parsed)
    (cltpt/base:convert-tree parsed cltpt/org-mode:*org-mode* cltpt/html:*html*)))

(test test-convert-2
  (let ((result (test-convert-2)))
    (fiveam:is (stringp result))
    (fiveam:is (> (length result) 0))))

(defun run-cltpt-tests ()
  "runs all defined fiveam test suites for cltpt."
  (format t "~&running cltpt tests...~%")
  (let ((results (run! 'cltpt-suite)))
    (unless results
      (explain! results))))

(defun transformer-test-1-func ()
  (let* ((full-string "[[mylink1-2:here1][testmore1- 2]]")
         (parsed
           `((:id org-link :begin 0 :end 33 :str ,full-string)
             ((:begin 0 :end 2 :str ,full-string))
             ((:id link-type :begin 2 :end 11 :str ,full-string))
             ((:begin 11 :end 12 :str ,full-string))
             ((:id link-dest :begin 12 :end 17 :str ,full-string))
             ((:begin 17 :end 19 :str ,full-string))
             ((:id link-desc :begin 19 :end 31 :str ,full-string))
             ((:begin 31 :end 33 :str ,full-string))))
         (dest-rule
           '(cltpt/combinator:consec
             "\\ref{"
             (:pattern (cltpt/combinator:symbol-matcher) :id link-dest)
             "}")))
    (cltpt/transformer:reconstruct-string-from-rule dest-rule parsed)))

(defun transformer-test-2-func ()
  (let* ((full-string "[[attachment:sliding]]")
         (parsed
           `((:id org-link :begin 0 :end 22 :str ,full-string)
             ((:begin 0 :end 2 :str ,full-string))
             ((:id link-dest :begin 2 :end 20 :str ,full-string))
             ((:begin 20 :end 22 :str ,full-string))))
         (dest-rule
           '(:pattern (cltpt/combinator:consec
                       "\\ref{"
                       (:pattern (cltpt/combinator:symbol-matcher)
                        :id link-dest)
                       "}")
             :id latex-link)))
    (cltpt/transformer:reconstruct-string-from-rule dest-rule parsed)))

(test transformer-test-2
  (fiveam:is
   (string= (cltpt/tests::transformer-test-2-func)
            "\\ref{attachment:sliding}")))

(test transformer-test-1
  (fiveam:is
   (string= (cltpt/tests::transformer-test-1-func)
            "\\ref{here1}")))

(defun test-combinator-number-1 ()
  (cltpt/combinator:match-rule
   nil
   '(cltpt/combinator:natural-number-matcher)
   "2023"
   0))

(test test-combinator-number-1
  (let ((result (test-combinator-number-1)))
    (fiveam:is (not (null result)))
    (fiveam:is (= (getf (car result) :end) 4))))

(defun roam-convert-test-1 ()
  (time
   (let* ((rmr (cltpt/roam:from-files
                '((:path ("/home/mahmooz/brain/notes/" "/home/mahmooz/brain/daily/")
                   :regex ".*\\.org"
                   :format "org-mode")))))
     (cltpt/roam:convert-all rmr (cltpt/base:text-format-by-name "html") "/tmp/out-%(identity title).html"))))

(defun roam-convert-test-2 ()
  (time
   (let* ((rmr (cltpt/roam:from-files
                '((:path ("/home/mahmooz/brain/notes/1710536040.org")
                   :regex ".*\\.org"
                   :format "org-mode")))))
     (cltpt/roam:convert-all rmr (cltpt/base:text-format-by-name "html") "/tmp/out-%(identity title).html"))))

(defun test-incremental-parsing-1 ()
  (let* ((text "- we have [[mylink]]
   a. nested item one \\(x=y\\)
      more nested text
      1. test1
      2. test2
   b. nested item two
 - item three")
         (obj (cltpt/base:parse
               cltpt/org-mode:*org-mode*
               text
               :text-object-types (list 'cltpt/org-mode::org-list 'cltpt/org-mode::org-link)))
         (list-obj (car (cltpt/base:text-object-children obj)))
         (old-list-obj-text (cltpt/base:text-object-text list-obj)))
    (format t "positions before: ~A~%"
            (loop for child in (cltpt/base:text-object-children list-obj)
                  collect (cltpt/base:text-object-begin-in-root child)))
    ;; this simply inserts a word at position 2
    (cltpt/base:handle-changed-regions
     list-obj
     (list 'cltpt/org-mode::org-list 'cltpt/org-mode::org-link)
     (list (cons "hello "
                 (make-region :begin 2 :end 2)))
     t)
    (format t "positions after: ~A~%"
            (loop for child in (cltpt/base:text-object-children list-obj)
                  collect (cltpt/base:text-object-begin-in-root child)))
    (format t
            "   === old ===~%~A~%   === new ===~%~A~%"
            old-list-obj-text
            (cltpt/base:text-object-text list-obj))))

(defun test-incremental-parsing-2 ()
  (let* ((text "some text \\(math here\\) here")
         (doc (cltpt/base:parse
               (cltpt/base:make-text-format "dummy")
               text
               :text-object-types (list 'cltpt/latex::inline-math)))
         (obj (car (cltpt/base:text-object-children doc)))
         (obj-text (cltpt/base:text-object-text obj)))
    (format t "num of children before: ~A~%" (length (cltpt/base:text-object-children doc)))
    ;; (cltpt/base::handle-change obj (list 'cltpt/latex::inline-math) 2 "\\(new math here\\)")
     (cltpt/base:handle-changed-regions
      doc
       (cltpt/base:make-text-format "dummy")
       (list
       (cons
        "\\(some\\) \\(math here\\)"
        (cltpt/base:make-region :begin 0 :end 21)))
      t)
    (format t
            "   === old ===~%~A~%   === new ===~%~A~%"
            obj-text
            (cltpt/base:text-object-text obj))
    (format t "num of children after: ~A~%" (length (cltpt/base:text-object-children doc)))
    (format t "updated doc: ~A~%" (cltpt/base:text-object-text doc))))

(defun test-incremental-parsing-3 ()
  (let* ((text "start \\(math here\\) here")
         (doc (cltpt/base:parse
               (cltpt/base:make-text-format "dummy")
               text
               :text-object-types (list 'cltpt/latex::inline-math)))
         (obj (car (cltpt/base:text-object-children doc)))
         (obj-text (cltpt/base:text-object-text obj)))
    (format t "num of children before: ~A~%"
            (length (cltpt/base:text-object-children doc)))
    (cltpt/base:handle-changed-regions
     doc
     cltpt/org-mode:*org-mode*
     (list
      (cons
       "some"
       (cltpt/base:region-incf
        (cltpt/base:make-region :begin 2 :end 6)
        (length "start ")))
      (cons
       "math"
       (cltpt/base:region-incf
        (cltpt/base:make-region :begin 7 :end 11)
        (length "start ")))
      (cons
       "\\(even more math\\) "
       (cltpt/base:region-incf
        (cltpt/base:make-region :begin 14 :end 14)
        (length "start "))))
     t)
    (format t "num of children after: ~A~%"
            (length (cltpt/base:text-object-children doc)))
    (format t
            "   === old ===~%~A~%   === new ===~%~A~%"
            obj-text
            (cltpt/base:text-object-text obj))
    (format t "updated doc: ~A~%" (cltpt/base:text-object-text doc))))

(defparameter *test-forest*
  (list (cons '(:text "projects" :expanded t)
              (list (cons '(:text "lisp" :expanded t)
                          (list '(:text "cltpt")))
                    '(:text "python")))
        (cons '(:text "documents" :expanded t)
              (list '(:text "resume.docx")))
        '(:text "downloads")))

(defun test-outline-1 ()
  (format t "~%--- 1. lsblk style ---~%~a"
          (cltpt/tree/outline:render-outline *test-forest*))
  (format t "~%--- 2. ascii style ---~%~a"
          (cltpt/tree/outline:render-outline *test-forest* cltpt/tree/outline:*ascii-style*))
  (format t "~%--- 3. simple style ---~%~a"
          (cltpt/tree/outline:render-outline *test-forest* cltpt/tree/outline:*simple-style*)))

(defun test-outline-2 ()
  (format t "~%--- 1. s-expression output ---~%~a~%"
          (cltpt/tree/outline:render-as-s-expression *test-forest*))
  (format t "~%--- 2. json output ---~%~a~%"
          (cltpt/tree/outline:render-as-json *test-forest*))
  (format t "~%--- 3. graphviz output ---~%")
  (format t "save this as 'tree.dot' and run: dot -Tpng tree.dot -o tree.png~%~a"
          (cltpt/tree/outline:render-as-dot *test-forest*))
  (format t "~%--- 4. path list output ---~%~a"
          (cltpt/tree/outline:render-as-path-list *test-forest*)))

(test test-incremental-parsing-1
  (let* ((text "- we have [[mylink]]
   a. nested item one \\(x=y\\)
      more nested text
      1. test1
      2. test2
   b. nested item two
 - item three")
         (obj (cltpt/base:parse
               cltpt/org-mode:*org-mode*
               text
               :text-object-types (list 'cltpt/org-mode::org-list 'cltpt/org-mode::org-link)))
         (list-obj (car (cltpt/base:text-object-children obj)))
         (result (cltpt/base:handle-changed-regions
                  list-obj
                  cltpt/org-mode:*org-mode*
                  (list (cons "hello "
                              (cltpt/base:make-region :begin 2 :end 2)))
                  t)))
    (fiveam:is
     (not (null result)))))

(test test-incremental-parsing-2
  (let* ((text "some text \\(math here\\) here")
         (doc (cltpt/base:parse
               (cltpt/base:make-text-format "dummy")
               text
               :text-object-types (list 'cltpt/latex::inline-math)))
         (result (cltpt/base:handle-changed-regions
                  doc
                  (make-text-format "dummy")
                  (list
                   (cons
                    "more text"
                    (cltpt/base:make-region :begin 10 :end 10)))
                  t)))
    (fiveam:is
     (listp result))))