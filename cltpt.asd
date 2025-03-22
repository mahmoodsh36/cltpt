(defsystem "cltpt"
  :description "cltpt - common lisp text processing tools, is a set of tools for working with different text formats."
  :version "0.1"
  :author "Mahmood Sheikh <mahmod.m2015@gmail.com>"
  :depends-on ("str" "uiop" "cl-ppcre" "cl-fad" "lparallel" "clingon" "ironclad")
  :components ((:file "cltpt")
               (:file "text-algorithms")
               (:file "text-object")
               (:file "org-mode")
               (:file "parser")
               (:file "utils")
               (:file "trees")
               (:file "tests")
               (:file "export")
               (:file "latex-export")
               (:file "html-export")
               (:file "latex-previews")
               (:file "commandline")))