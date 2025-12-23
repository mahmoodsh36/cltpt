(defpackage :cltpt/tests/incremental
  (:use :cl :it.bese.fiveam)
  (:export :run-incremental-tests))

(in-package :cltpt/tests/incremental)

(def-suite incremental-suite
  :description "tests for incremental parsing functionality."
  :in cltpt/tests::cltpt-suite)

(in-suite incremental-suite)

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
                 (cltpt/buffer:make-region :begin 2 :end 2)))
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
    (cltpt/base:handle-changed-regions
     doc
     (cltpt/base:make-text-format "dummy")
     (list
      (cons
       "\\(some\\) \\(math here\\)"
       (cltpt/buffer:make-region :begin 0 :end 21)))
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
         (initial-children-count (length (cltpt/base:text-object-children doc)))
         (obj (car (cltpt/base:text-object-children doc)))
         (obj-text (cltpt/base:text-object-text obj)))
    (format t "num of children before: ~A~%" initial-children-count)
    (cltpt/base:handle-changed-regions
     doc
     cltpt/org-mode:*org-mode*
     (list
      (cons
       "some"
       (cltpt/buffer:region-incf
        (cltpt/buffer:make-region :begin 2 :end 6)
        (length "start ")))
      (cons
       "math"
       (cltpt/buffer:region-incf
        (cltpt/buffer:make-region :begin 7 :end 11)
        (length "start ")))
      (cons
       "\\(even more math\\) "
       (cltpt/buffer:region-incf
        (cltpt/buffer:make-region :begin 14 :end 14)
        (length "start "))))
     t)
    (let ((final-children-count (length (cltpt/base:text-object-children doc))))
      (format t "num of children after: ~A~%" final-children-count)
      (format t
              "   === old ===~%~A~%   === new ===~%~A~%"
              obj-text
              (cltpt/base:text-object-text obj))
      (format t "updated doc: ~A~%" (cltpt/base:text-object-text doc))
      (values initial-children-count final-children-count))))

(test test-incremental-parsing-3
  (multiple-value-bind (initial-children-count final-children-count)
      (test-incremental-parsing-3)
    (fiveam:is (= final-children-count 2)
               "document should have 2 children after the operation")))

(defun test-incremental-parsing-4 ()
  (let* ((text "** header test
some1 \\(math\\) here."
               )
         (doc (cltpt/base:parse cltpt/org-mode:*org-mode* text))
         (obj (car (cltpt/base:text-object-children doc)))
         (initial-children-count (length (cltpt/base:text-object-children obj)))
         (obj-text (cltpt/base:text-object-text obj)))
    (format t "num of children before: ~A~%" initial-children-count)
    (cltpt/base:handle-changed-regions
     doc
     cltpt/org-mode:*org-mode*
     (list
      (cons
       "\\(some2 math\\) "
       (cltpt/buffer:make-region :begin 3 :end 3))
      (cons
       "\\(some more math\\) "
       (cltpt/buffer:make-region :begin 15 :end 15))
      (cons
       "changed math"
       (cltpt/buffer:make-region :begin 23 :end 27)))
     t)
    (setf obj (car (cltpt/base:text-object-children doc)))
    (let ((final-children-count (length (cltpt/base:text-object-children obj))))
      ;; should be 2 (or 3 if matching in header title is to be properly implemented later)
      (format t "num of children after: ~A~%" final-children-count)
      (format t
              "   === old ===~%~A~%   === new ===~%~A~%"
              obj-text
              (cltpt/base:text-object-text doc))
      (values initial-children-count final-children-count))))

(defun run-incremental-tests ()
  (format t "~&running incremental parsing tests...~%")
  (let ((results (run! 'incremental-suite)))
    (unless results
      (explain! results))))