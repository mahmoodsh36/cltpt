(defpackage :cltpt/tests
  (:use :cl :it.bese.fiveam)
  (:import-from
   :cltpt/tests/utils
   :plist-to-match
   :string=+diff
   :match-tree-equal-p
   :match-trees-equal-p
   :match-tree-diff
   :is-match-tree
   :is-match-trees)
  (:export
   :run-cltpt-tests
   :cltpt-suite
   :plist-to-match
   :string=+diff
   :match-tree-equal-p
   :match-trees-equal-p
   :match-tree-diff
   :is-match-tree
   :is-match-trees))

(in-package :cltpt/tests)

;; parent test suite, all other suites are children of this one
(def-suite cltpt-suite
  :description "parent test suite for all cltpt tests.")

(defun run-cltpt-tests ()
  (format t "~&running all cltpt tests...~%")
  ;; set up relative paths for latex previews so tests work across different systems
  (setf cltpt/latex-previews::*latex-previews-tmp-directory* "cltpt-latex-previews/tmp/")
  (setf cltpt/latex-previews::*latex-previews-cache-directory* "cltpt-latex-previews/cache/")
  ;; ensure directories exist
  (ensure-directories-exist cltpt/latex-previews::*latex-previews-tmp-directory*)
  (ensure-directories-exist cltpt/latex-previews::*latex-previews-cache-directory*)
  (cltpt/zoo:init)
  (let ((results (run! 'cltpt-suite)))
    (unless results
      (explain! results))))