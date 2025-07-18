(defpackage :cltpt/html
  (:use :cl :cltpt/base :cltpt/latex)
  (:export :html))

(in-package :cltpt/html)

(defun make-html ()
  (make-text-format
   "html"
   '(display-math inline-math latex-env
     text-macro post-lexer-text-macro)))

(defvar html)
(eval-when (:load-toplevel :execute)
  (setf html (make-html)))

;; (defmethod text-format-escape ((fmt (eql html)) text escapable-chars)
;;   text)