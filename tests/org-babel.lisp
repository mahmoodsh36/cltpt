(defpackage :cltpt/tests/org-babel
  (:use :cl)
  (:import-from
   :cltpt/tests
   :string=+diff))

(in-package :cltpt/tests/org-babel)

(fiveam:def-suite org-babel-suite
  :description "tests for org-babel functionality."
  :in cltpt/tests::cltpt-suite)

(fiveam:in-suite org-babel-suite)

(defun test-org-babel-1 ()
  (let* ((cltpt/org-mode::*org-enable-babel* t)
         (parsed (cltpt/base:parse-file cltpt/org-mode:*org-mode* "tests/babel-python.org"))
         (actual-output (cltpt/base:convert-document cltpt/org-mode:*org-mode* cltpt/html:*html* parsed)))
    actual-output))