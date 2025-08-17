(require 'asdf)
(require 'uiop)
(pushnew :commandline *features*)
(pushnew #p"./" asdf:*central-registry* :test #'equal)
(asdf:load-system "cltpt" :force t) ;; force recompilation
(cltpt/zoo:init)
;; (asdf:load-system "cltpt")
;; (in-package :cltpt)
#+commandline
(cltpt/commandline:commandline-main (uiop:command-line-arguments))