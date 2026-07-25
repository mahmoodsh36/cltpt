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
         (parsed (cltpt/base:parse-file cltpt/org-mode:*org-mode* "tests/babel.org"))
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
    <div class='org-src-block-container'><div class='org-src' data-lang='python'><pre><code>text = &quot;block1&quot;
print(text)</code></pre></div></div>
<br>
block1
<br>
<div class='org-src-block-container'><div class='org-src' data-lang='python' data-name='blk0'><pre><code>text = &quot;block2&quot;
print(text)
print(text)</code></pre></div></div>
<br>
block2
<br>
block2
<br>
<div class='org-src-block-container'><div class='org-src' data-lang='python' data-name='blk1'><pre><code>filepath = &quot;out.png&quot;
print(filepath)
</code></pre></div>

<div class='org-babel-results'><img src='out.png' /></div></div>
<br>
<div class='org-src-block-container'><div class='org-src' data-lang='python' data-name='image-handler'><pre><code>return { &quot;path&quot;: &quot;link-path&quot;, &quot;type&quot;: &quot;link-type&quot; }</code></pre></div></div>
<br>
<div class='org-src-block-container'><div class='org-src' data-lang='python' data-name='blk8'><pre><code>return [1, 2, 3]</code></pre></div></div>
<br>
[1, 2, 3]
<br>
<div class='org-src-block-container'><div class='org-src' data-lang='python' data-var='a1=blk8' data-name='word-handler'><pre><code>print(&apos;word &apos; + str(a1))</code></pre></div></div>
<br>
word [1, 2, 3]
<br>
<div class='org-src-block-container'><div class='org-src' data-lang='python' data-name='blk0-child-handler'><pre><code>child = [%(getf child :match), %(getf child :begin), %(getf child :end)]
return child</code></pre></div></div>
<br>
<div class='org-src-block-container'><div class='org-src' data-lang='python' data-name='blk0-tree-handler'><pre><code>class blk0_tree:
    def __init__(self):
        self.prop = 1

tree = blk0_tree()
%&apos;blk0-child-handler-loop</code></pre></div></div>
<br>
<div class='org-src-block-container'><div class='org-src' data-lang='python' data-pattern='(cltpt/combinator:separated-atleast-one ,(string #\\newline) (cltpt/combinator:all-but-newline)))' data-input-rule='%'(:name blk0-tree' data-input-handler='blk0 word-handler' data-input-type='blk0 stream' data-input='blk0' data-input-handler='blk1 org-link image-handler' data-input-type='blk1 object' data-input='blk1'><pre><code>print(blk1)
for word in %&apos;blk0-tree-handler:
    print(word)</code></pre></div></div>
<br>
<div class='org-src-block-container'><div class='org-src' data-lang='c'><pre><code>printf(&quot;hello from C\\n&quot;);</code></pre></div></div>
<br>
hello from C
<br>
<div class='org-src-block-container'><div class='org-src' data-lang='python' data-name='pynums'><pre><code>return [10, 20, 30]</code></pre></div></div>
<br>
[10, 20, 30]
<br>
<div class='org-src-block-container'><div class='org-src' data-lang='c' data-var='xs=pynums'><pre><code>int total = 0;
for (unsigned int i = 0; i &lt; xs_len; i++) total += xs[i];
printf(&quot;sum of %lu items = %ld\\n&quot;, xs_len, total);</code></pre></div></div>
<br>
sum of 3 items = 60
<br>
<div class='org-src-block-container'><div class='org-src' data-lang='c' data-main='no'><pre><code>#include &lt;stdio.h&gt;
int main(void) {
    printf(&quot;unwrapped %d\\n&quot;, 99);
    return 0;
}</code></pre></div></div>
<br>
unwrapped 99
<br>

  </div>
</body>
</html>"
                          ))
    (values actual-output expected-output)))

(fiveam:test test-org-babel-1
  (multiple-value-bind (actual-output expected-output)
      (test-org-babel-1-func)
    (fiveam:is (string=+diff actual-output
                             expected-output
                             "babel test should match expected output")
               "babel test should match expected output")))
