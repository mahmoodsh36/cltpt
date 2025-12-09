#!/usr/bin/env -S sbcl --script
(require 'asdf)
(require 'uiop)
(pushnew #p"./" asdf:*central-registry* :test #'equal)
(asdf:load-system "cltpt" :force t) ;; force recompilation
(cltpt/tests::run-cltpt-tests)