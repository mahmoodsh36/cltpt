#!/usr/bin/env -S sbcl --script
(require 'asdf)
(require 'uiop)
(pushnew #p"./" asdf:*central-registry* :test #'equal)
(asdf:load-system "cltpt" :force t) ;; force recompilation

(setf (getf cltpt:*debug* :convert) t)
(setf (getf cltpt:*debug* :parse) t)
(setf (getf cltpt:*debug* :roam) t)

(cltpt/zoo:init)

(format t "~%running cltpt tests...~%~%")
(let ((result1 (cltpt/tests::run-cltpt-tests)))
  (format t "~%~%running org-mode tests...~%~%")
  (let ((result2 (cltpt/tests/org-mode::run-org-mode-tests)))
    (if (and result1 result2)
        (uiop:quit 0)
        (uiop:quit 1))))
