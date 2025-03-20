(require 'asdf)
(pushnew "./" asdf:*central-registry* :test #'equal)
(asdf:load-system "cltpt")
(cltpt::commandline-main (uiop:command-line-arguments))