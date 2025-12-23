(defpackage :cltpt/tests/buffer
  (:use :cl :it.bese.fiveam :cltpt/buffer)
  (:export :run-buffer-tests))

(in-package :cltpt/tests/buffer)

(def-suite buffer-suite
  :description "tests for cltpt/buffer."
  :in cltpt/tests::cltpt-suite)

(in-suite buffer-suite)

(test buffer-test-new-level
  (let* ((parent (make-buffer :text "section A: [content here] | section B: [footer]"))
         (child (make-buffer :parent parent :region (make-region :begin 11 :end 25))))

    ;; scheduled changes
    ;; replace 'content' (1-8) with 'data'
    (schedule-change child 1 8 "data")
    ;; insert 'raw ' at 1
    (schedule-change child 1 1 "raw ")
    ;; delete brackets
    (schedule-change child 0 1 "")
    (schedule-change child 13 14 "")

    (schedule-change child 1 13 #'string-upcase :new-level t)

    ;; apply changes after the immediate edits
    (apply-scheduled-changes child :propagate-to :root)

    ;; might we want the result to be RAW DATA HERE? because technically the insertion of
    ;; "raw " at 1-1 is also kinda part of the original region 1-13
    (is (string= (buffer-text child) "raw DATA HERE"))
    (is (string= (buffer-text parent) "section A: raw DATA HERE | section B: [footer]"))))

(test buffer-test-simple
  (let ((buf (make-buffer :text "0123456789")))
    (schedule-change buf 8 10 "END")
    (buffer-insert buf 0 "PRE")
    (apply-scheduled-changes buf)
    (is (string= (buffer-text buf) "PRE01234567END"))))

(defun buffer-test-1-func ()
  (let* ((parent (make-buffer :text "AAABBBCCC"))
         (buf-a (make-buffer :parent parent :region (make-region :begin 0 :end 3)))
         (buf-b (make-buffer :parent parent :region (make-region :begin 3 :end 6)))
         (buf-c (make-buffer :parent parent :region (make-region :begin 6 :end 9))))
    (buffer-replace buf-a 0 3 "AAAAA" :propagate t))
  (let ((buf (make-buffer :text "0123456789")))
    (schedule-change buf 2 5 "X" :new-level t)
    (schedule-change buf 4 6 "Y")
    (apply-scheduled-changes buf))
  (let ((buf (make-buffer :text "Hello World")))
    (schedule-change buf 6 6 "Big " :new-level t)
    (schedule-change buf 6 11 "Universe" :new-level t)
    (apply-scheduled-changes buf))
  (let ((buf (make-buffer :text "lower CASE")))
    (schedule-change buf 0 5 #'string-upcase :new-level t)
    (schedule-change buf 6 10 #'string-downcase :new-level nil)
    (apply-scheduled-changes buf))
  (let ((buf (make-buffer :text "012345")))
    (schedule-change buf 4 6 "END")
    (buffer-insert buf 0 "PRE" :propagate nil)
    (apply-scheduled-changes buf))
  (let* ((parent (make-buffer :text "OriginalContent"))
         (child (make-buffer :parent parent :region (make-region :begin 0 :end 8))))
    (buffer-replace child 0 8 "Garbage" :propagate nil)
    (buffer-fetch-parent-text child)))

(test buffer-test-1
  (is (string= (buffer-test-1-func) "Original")))

(defun buffer-test-2-func ()
  (let* ((parent (make-buffer :text "AAAA [CHILD] BBBB"))
         (child (make-buffer :parent parent :region (make-region :begin 5 :end 12))))
    (schedule-change parent 0 2 "XX")
    (schedule-change parent 2 4 "YY")
    (schedule-change parent 0 5 "NEW " :discard-contained t)
    (apply-scheduled-changes parent)
    (schedule-change parent 5 10 "HI")
    (schedule-change parent 5 10 "LO" :discard-contained t)
    (apply-scheduled-changes child :propagate-to :root)
    (buffer-text parent)))

(test buffer-test-2
  (is (string= (buffer-test-2-func) "NEW [LO] BBBB")))

(defun run-buffer-tests ()
  (run! 'buffer-suite))