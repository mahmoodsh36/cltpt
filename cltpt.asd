(defsystem "cltpt"
  :description "cltpt - common lisp text processing tools, is a set of tools for working with different text formats."
  :version "0.1"
  :author "Mahmood Sheikh <mahmod.m2015@gmail.com>"
  :depends-on ("uiop" "ironclad" "fiveam" "local-time" "clingon")
  :components ((:file "cltpt")
               (:module "file-utils"
                :pathname "src/"
                :components ((:file "file-utils")))
               (:module "tree"
                :pathname "src/tree"
                :components ((:file "tree")))
               (:module "outline"
                :pathname "src/tree"
                :depends-on ("tree")
                :components ((:file "outline")))
               (:module "combinator"
                :pathname "src/combinator"
                :depends-on ("tree")
                :components ((:file "utils")
                             (:file "combinator")))
               (:module "base"
                :pathname "src/base/"
                :depends-on ("combinator" "file-utils")
                :components ((:file "base")
                             (:file "utils")
                             (:file "region")
                             (:file "text-object")
                             (:file "link")
                             (:file "text-format")
                             (:file "parser")
                             (:file "transform")
                             (:file "convert")))
               (:module "latex"
                :pathname "src/format/latex/"
                :depends-on ("base" "combinator" "file-utils")
                :components ((:file "latex")
                             (:file "latex-previews")
                             (:file "utils")))
               (:module "html"
                :pathname "src/format/html/"
                :depends-on ("base")
                :components ((:file "html")))
               (:module "roam"
                :pathname "src/extra/"
                :depends-on ("base" "file-utils")
                :components ((:file "roam")))
               (:module "agenda"
                :pathname "src/extra/agenda"
                :depends-on ("base" "roam")
                :components ((:file "time")
                             (:file "state")
                             (:file "task")
                             (:file "agenda")))
               (:module "org-mode"
                :pathname "src/format/org-mode"
                :depends-on ("combinator" "file-utils" "base" "latex" "html")
                :components ((:file "org-mode")
                             (:file "org-list")
                             (:file "org-table")))
               (:module "zoo"
                :pathname "src/"
                :depends-on ("base" "latex" "html" "org-mode" "roam")
                :components ((:file "zoo")))
               (:module "commandline"
                :pathname "src/extra"
                :depends-on ("base" "file-utils" "latex" "org-mode" "html")
                :components ((:file "commandline")))
               (:module "tests"
                :pathname "tests/"
                :depends-on ("base" "org-mode")
                :components ((:file "more")
                             (:file "org-mode")
                             (:file "manual")))))