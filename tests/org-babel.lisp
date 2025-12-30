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

(defun test-org-babel-1-func ()
  (let* ((cltpt/org-mode::*org-enable-babel* t)
         (parsed (cltpt/base:parse-file cltpt/org-mode:*org-mode* "tests/babel-python.org"))
         (actual-output (cltpt/base:convert-document
                         cltpt/org-mode:*org-mode*
                         cltpt/html:*html*
                         parsed))
         (expected-output "<!DOCTYPE html>
<html>
<head>
  <meta charset=\"UTF-8\">
  <title> NIL </title>
</head>
<body>
  <div class=\"post-content\">
    <h1> NIL - NIL </h1>
    <div class='org-src' data-lang='python'><pre><code>  text = &quot;block1&quot;
  print(text)</code></pre></div>
#+RESULTS:
block1

#+RESULTS:
block1

<br>
<div class='org-src' data-lang='python'><pre><code>  text = &quot;block2&quot;
  print(text)</code></pre></div>
#+RESULTS:
block2

#+RESULTS:
block2

<br>
<div class='org-src' data-lang='python' data-reconstruct='(PATTERN
                   (ANY
                    (CONSEC [[ (PATTERN (SYMBOL-MATCHER) ID LINK-TYPE) :
                            (PATTERN (ALL-BUT []) ID LINK-DEST) ][
                            (PATTERN (ALL-BUT []) ID LINK-DESC) ]])
                    (CONSEC [[ (PATTERN (SYMBOL-MATCHER) ID LINK-TYPE) :
                            (PATTERN (ALL-BUT []) ID LINK-DEST) ]])
                    (CONSEC [[ (PATTERN (ALL-BUT []) ID LINK-DEST) ]]))
                   ON-CHAR [)'><pre><code>  filepath = &quot;out.png&quot;
  print(filepath)</code></pre></div>
#+RESULTS:
[[out.png]]
#+RESULTS:
[[out.png]]  </div>
</body>
</html>"
                          ))
    (values actual-output expected-output)))

(fiveam:test test-org-babel-1
  (multiple-value-bind (actual-output expected-output)
      (test-org-babel-1-func)
    (fiveam:is (string=+diff actual-output
                             expected-output
                             "babel python test should match expected output")
               "babel python test should match expected output")))