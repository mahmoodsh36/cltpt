(defsystem "cltpt"
  :description "cltpt - common lisp text processing tools, is a set of tools for working with different text formats."
  :version "0.1"
  :author "Mahmood Sheikh <mahmod.m2015@gmail.com>"
  :depends-on ("str" "uiop" "cl-ppcre" "cl-fad" "lparallel" "clingon" "ironclad")
  :components ((:file "cltpt")
               (:module "base"
                :pathname "src/"
                :components ((:file "text-algorithms")
                             (:file "text-object")
                             (:file "text-format")
                             (:file "parser")
                             (:file "utils")
                             (:file "trees")
                             (:file "convert")
                             (:file "latex")
                             (:file "html")
                             (:file "tests")))
               (:module "commandline"
                :pathname "src/commandline"
                :components ((:file "commandline")))
               (:module "org-mode"
                :pathname "src/org-mode"
                :components ((:file "org-mode")
                             (:file "latex-previews")
                             (:file "latex-export")
                             (:file "html-export")))))