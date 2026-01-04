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
    
<br>
block1
<br>

<br>
block2
<br>
block2
<br>
<div class='org-src' data-lang='python' data-name='blk1' data-reconstruct='(PATTERN
                   (ANY
                    (CONSEC [[ (PATTERN (SYMBOL-MATCHER) ID LINK-TYPE) :
                            (PATTERN (ALL-BUT []) ID LINK-DEST) ][
                            (PATTERN (ALL-BUT []) ID LINK-DESC) ]])
                    (CONSEC [[ (PATTERN (SYMBOL-MATCHER) ID LINK-TYPE) :
                            (PATTERN (ALL-BUT []) ID LINK-DEST) ]])
                    (CONSEC [[ (PATTERN (ALL-BUT []) ID LINK-DEST) ]]))
                   ON-CHAR [)'><pre><code>  filepath = &quot;out.png&quot;
  print(filepath)
</code></pre></div>

<div class='org-babel-results'><img src='out.png' /></div>
<br>

<br>

<br>

<br>

<br>
<div class='org-src' data-lang='python' data-pattern='(cltpt/combinator:separated-atleast-one ,(string #\\newline) (cltpt/combinator:all-but-newline)))' data-input-rule='%'(:name blk0-tree' data-input-handler='blk0 word-handler' data-input-type='blk0 stream' data-input='blk0' data-input-handler='blk1 org-link image-handler' data-input-type='blk1 object' data-input='blk1'><pre><code>  print(blk1)
  for word in %&apos;blk0-tree-handler:
      print(word)</code></pre></div>  </div>
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