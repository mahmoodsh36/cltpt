(defpackage :cltpt
  (:use :common-lisp :asdf))
(in-package :cltpt)

(defsystem "cltpt"
  :description "cltptt - common lisp text processing tools, is a set of tools for working with different text formats."
  :version "0.1"
  :author "Mahmood Sheikh <mahmod.m2015@gmail.com>"
  :depends-on ("str" "uiop" "cl-ppcre" "cl-fad")
  :components ((:file "text-algorithms")
               (:file "parser")
               (:file "org-mode-parser")
               (:file "text-object")
               (:file "utils")
               (:file "trees")
               ))