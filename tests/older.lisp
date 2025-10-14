(in-package :cltpt/tests)

;; ill keep these here for now, they might be useful in the future, i dont
;; want to delete them

(defun test-bind-and-eval-1 ()
  (cltpt/base:bind-and-eval
   `((title "mytitle"))
   (lambda ()
     (format t "the title is: ~A~%" title))
   :cltpt/tests))

(defun test-bind-and-eval-2 ()
  (cltpt/base:bind-and-eval
   `((title "mytitle"))
   (lambda ()
     (format t "the title is: ~A~%" title))
   :cltpt/tests))