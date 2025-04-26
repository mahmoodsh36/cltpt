(defsystem "cltpt"
  :description "cltpt - common lisp text processing tools, is a set of tools for working with different text formats."
  :version "0.1"
  :author "Mahmood Sheikh <mahmod.m2015@gmail.com>"
  :depends-on ("str" "uiop" "cl-ppcre" "cl-fad" "clingon" "ironclad" "fiveam")
  :components ((:file "cltpt")
               (:module "base"
                :pathname "src/"
                :components ((:file "utils")
                             (:file "text-object")
                             (:file "text-algorithms")
                             (:file "text-format")
                             (:file "parser")
                             (:file "latex")
                             (:file "html")
                             (:file "trees")
                             (:file "convert")
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