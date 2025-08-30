(defsystem "cltpt"
  :description "cltpt - common lisp text processing tools, is a set of tools for working with different text formats."
  :version "0.1"
  :author "Mahmood Sheikh <mahmod.m2015@gmail.com>"
  :depends-on ("uiop" "cl-ppcre" "ironclad" "fiveam" "local-time" "clingon")
  :components ((:file "cltpt")
               (:module "file-utils"
                :pathname "src/"
                :components ((:file "file-utils")))
               (:module "tree"
                :pathname "src/"
                :components ((:file "tree")))
               (:module "combinator"
                :pathname "src/combinator"
                :depends-on ("tree")
                :components ((:file "utils")
                             (:file "combinator")))
               (:module "base"
                :pathname "src/base/"
                :depends-on ("combinator" "file-utils")
                :components ((:file "base")
                             (:file "parser")
                             (:file "transformer")
                             (:file "utils")
                             (:file "text-object")
                             (:file "text-format")
                             (:file "convert")))
               (:module "latex"
                :pathname "src/latex/"
                :depends-on ("base" "combinator" "file-utils")
                :components ((:file "latex")
                             (:file "latex-previews")
                             (:file "utils")))
               (:module "html"
                :pathname "src/html/"
                :depends-on ("base")
                :components ((:file "html")))
               (:module "roam"
                :pathname "src/extra/"
                :depends-on ("base" "file-utils")
                :components ((:file "roam")))
               (:module "agenda"
                :pathname "src/extra/"
                :depends-on ("base" "roam")
                :components ((:file "agenda")))
               (:module "org-mode"
                :pathname "src/org-mode"
                :depends-on ("combinator" "file-utils" "base" "latex" "html")
                :components ((:file "org-mode")
                             (:file "org-list")
                             (:file "org-table")))
               (:module "zoo"
                :pathname "src/"
                :depends-on ("base" "latex" "html" "org-mode")
                :components ((:file "zoo")))
               (:module "commandline"
                :pathname "src/extra"
                :depends-on ("base" "file-utils" "latex" "org-mode" "html")
                :components ((:file "commandline")))
               (:module "tests"
                :pathname "tests/"
                :depends-on ("base" "org-mode")
                :components ((:file "parse")))))