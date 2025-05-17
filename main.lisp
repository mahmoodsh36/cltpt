(require 'asdf)
(require 'uiop)
(pushnew #p"./" asdf:*central-registry* :test #'equal)
(asdf:load-system "cltpt" :force t) ;; force recompilation
;; (asdf:load-system "cltpt")
(in-package :cltpt)
(cltpt/commandline:commandline-main (uiop:command-line-arguments))