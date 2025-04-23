(require 'asdf)
(require 'uiop)
(pushnew #p"./" asdf:*central-registry* :test #'equal)
(asdf:load-system "cltpt" :force t) ;; force recompilation
;; (asdf:load-system "cltpt")
(cltpt::commandline-main (uiop:command-line-arguments))