(defsystem "cltpt"
  :description "cltpt - common lisp text processing tools, is a set of tools for working with different text formats."
  :version "0.1"
  :author "Mahmood Sheikh <mahmod.m2015@gmail.com>"
  :depends-on ("str" "uiop" "cl-ppcre" "cl-fad" "clingon" "ironclad" "fiveam")
  :components ((:file "cltpt")
               (:module "combinator"
                :pathname "src/"
                :components ((:file "combinator")))
               (:module "base"
                :pathname "src/base/"
                :depends-on ("combinator")
                :components ((:file "base")
                             (:file "parser")
                             (:file "transformer")
                             (:file "utils")
                             (:file "text-object")
                             (:file "text-algorithms")
                             (:file "text-format")
                             (:file "convert")))
               (:module "latex"
                :pathname "src/latex/"
                :depends-on ("base" "combinator")
                :components ((:file "latex")
                             (:file "text-objects")
                             (:file "latex-previews")
                             (:file "utils")))
               (:module "html"
                :pathname "src/html/"
                :depends-on ("base")
                :components ((:file "html")))
               (:module "agenda"
                :pathname "src/extra/"
                :depends-on ("base")
                :components ((:file "agenda")))
               (:module "roam"
                :pathname "src/extra/"
                :depends-on ("base")
                :components ((:file "roam")))
               (:module "org-mode"
                :pathname "src/org-mode"
                :depends-on ("combinator" "base" "latex" "html")
                :components ((:file "org-mode")
                             (:file "org-list")
                             (:file "org-table")))
               (:module "zoo"
                :pathname "src/"
                :depends-on ("base")
                :components ((:file "zoo")))
               (:module "commandline"
                :pathname "src/commandline"
                :depends-on ("base" "latex" "org-mode" "html")
                :components ((:file "commandline")))
               (:module "tests"
                :pathname "tests/"
                :depends-on ("base" "org-mode")
                :components ((:file "parse")))
               ))