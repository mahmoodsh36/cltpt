#!/usr/bin/env -S sbcl --script
(load (sb-ext:posix-getenv "ASDF"))
(pushnew "./" asdf:*central-registry* :test #'equal)
(asdf:load-system "cltpt")
(cltpt::commandline-main (uiop:command-line-arguments))