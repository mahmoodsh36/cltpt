(defsystem "cltpt"
  :description "cltpt - common lisp text processing tools, is a set of tools for working with different text formats."
  :version "0.1"
  :author "Mahmood Sheikh <mahmod.m2015@gmail.com>"
  :depends-on ("uiop" "ironclad" "fiveam" "local-time" "clingon" "bordeaux-threads")
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
                :components ((:file "match")
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
               (:module "latex-previews"
                :pathname "src/extra/"
                :depends-on ("base" "file-utils")
                :components ((:file "latex-previews")))
               (:module "latex"
                :pathname "src/format/latex/"
                :depends-on ("base" "combinator" "file-utils")
                :components ((:file "latex")
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
               ;; TODO: org-mode should not directly depend on roam/agenda.
               (:module "org-mode"
                :pathname "src/format/org-mode"
                :depends-on ("combinator" "file-utils" "base" "latex" "html" "latex-previews")
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
               (:module "babel"
                :pathname "src/babel"
                :depends-on ("tree")
                :components ((:file "babel")
                             (:file "python")))
               (:module "tests"
                :pathname "tests/"
                :depends-on ("base" "org-mode" "latex-previews")
                :components ((:file "more")
                             (:file "org-mode")
                             (:file "manual")))))