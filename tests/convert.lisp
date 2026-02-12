(defpackage :cltpt/tests/convert
  (:use :cl :it.bese.fiveam)
  (:import-from
   :cltpt/tests
   :string=+diff)
  (:export
   :run-conversion-tests
   :test-org-src-block-with-image-result-func
   :test-comprehensive-org-document-func))

(in-package :cltpt/tests/convert)

(def-suite conversion-suite
  :description "tests for format conversion functionality."
  :in cltpt/tests::cltpt-suite)

(in-suite conversion-suite)

(defun test-org-src-block-with-image-result-func ()
  (cltpt/base:parse
   cltpt/org-mode:*org-mode*
   "#+begin_src python :results file :exports both :eval no
import matplotlib.pyplot as plt
import numpy as np

x = np.linspace(0, 10, 100)
y = np.sin(x)
plt.plot(x, y)
plt.savefig('plot.png')
plt.close()
#+end_src

#+RESULTS:
[[file:plot.png]]"))

(defun org-src-block-with-image-result-html-conversion ()
  (let* ((doc (test-org-src-block-with-image-result-func))
         (html-output (cltpt/base:convert-tree doc cltpt/org-mode:*org-mode* cltpt/html:*html*)))
    html-output))

(test test-org-src-block-with-image-result-html-conversion
  (let* ((html-output (org-src-block-with-image-result-html-conversion))
         (expected-html "<div class='org-src-block-container'><div class='org-src' data-lang='python' data-eval='no'><pre><code>import matplotlib.pyplot as plt
import numpy as np

x = np.linspace(0, 10, 100)
y = np.sin(x)
plt.plot(x, y)
plt.savefig(&apos;plot.png&apos;)
plt.close()
</code></pre></div>

<div class='org-babel-results'><img src='plot.png' /></div></div>"))
    (is (string=+diff
         html-output
         expected-html
         "HTML conversion of src block with image result should match expected output")
        "HTML conversion of src block with image result should match expected output")))

(defun test-comprehensive-org-document-func ()
  "test parsing a comprehensive org document with many features."
  (let ((content (uiop:read-file-string "tests/data/comprehensive-org-test.org")))
    (cltpt/base:parse cltpt/org-mode:*org-mode* content)))

(defun comprehensive-org-document-html-conversion ()
  (let* ((doc (test-comprehensive-org-document-func))
         (html-output (cltpt/base:convert-document cltpt/org-mode:*org-mode* cltpt/html:*html* doc)))
    html-output))

(test test-comprehensive-org-document-html-conversion
  (let* ((html-output (comprehensive-org-document-html-conversion))
         (expected-html (uiop:read-file-string "tests/data/comprehensive-org-test-expected.html")))
    (is (string=+diff
         html-output
         expected-html
         "HTML conversion of comprehensive-org-test.org should match expected output")
        "HTML conversion of comprehensive-org-test.org should match expected output")))

(defun text-block-test-conversion-func ()
  "Test conversion of text-block-test.txt using simple format and compare with expected output."
  (let* ((content (uiop:read-file-string "tests/data/text-block-test.txt"))
         (parsed (cltpt/base:parse cltpt/base:*simple-format* content))
         (actual-output (cltpt/base:convert-document cltpt/base:*simple-format* cltpt/base:*simple-format* parsed))
         (expected-output (uiop:read-file-string "tests/data/text-block-test-expected.txt")))
    (values actual-output expected-output)))

(test text-block-test-conversion
  (multiple-value-bind (actual-output expected-output)
      (text-block-test-conversion-func)
    (is (string=+diff
         actual-output
         expected-output
         "simple format conversion of text-block-test.txt should match expected output")
        "simple format conversion of text-block-test.txt should match expected output")))

(defun test-org-html-conversion-func ()
  "Test conversion of test.org to HTML and compare with expected output."
  (let* ((content (uiop:read-file-string "tests/test.org"))
         (parsed (cltpt/base:parse cltpt/org-mode:*org-mode* content))
         (actual-output (cltpt/base:convert-document cltpt/org-mode:*org-mode* cltpt/html:*html* parsed))
         (expected-output (uiop:read-file-string "tests/data/test-org-expected.html")))
    (values actual-output expected-output)))

(test test-org-html-conversion
  (multiple-value-bind (actual-output expected-output)
      (test-org-html-conversion-func)
    (is (string=+diff
         actual-output
         expected-output
         "HTML conversion of test.org should match expected output")
        "HTML conversion of test.org should match expected output")))

(defun test-org-latex-conversion-func ()
  "Test conversion of test.org to LaTeX and compare with expected output."
  (let* ((content (uiop:read-file-string "tests/test.org"))
         (parsed (cltpt/base:parse cltpt/org-mode:*org-mode* content))
         (actual-output (cltpt/base:convert-document cltpt/org-mode:*org-mode* cltpt/latex:*latex* parsed))
         (expected-output (uiop:read-file-string "tests/data/test-org-expected.tex")))
    (values actual-output expected-output)))

(test test-org-latex-conversion
  (multiple-value-bind (actual-output expected-output)
      (test-org-latex-conversion-func)
    (is (string=+diff
         actual-output
         expected-output
         "LaTeX conversion of test.org should match expected output")
        "LaTeX conversion of test.org should match expected output")))

(defun run-conversion-tests ()
  (format t "~&running conversion tests...~%")
  (let ((results (run! 'conversion-suite)))
    (unless results
      (explain! results))))