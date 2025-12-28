(defsystem "cltpt"
  :description "cltpt - common lisp text processing tools, is a set of tools for working with different text formats."
  :version "0.1"
  :author "Mahmood Sheikh <mahmod.m2015@gmail.com>"
  :depends-on ("uiop" "ironclad" "fiveam" "local-time" "clingon" "bordeaux-threads")
  :components ((:file "cltpt")
               (:module "file-utils"
                :pathname "src/"
                :components ((:file "file-utils")))
               (:module "str-utils"
                :pathname "src/"
                :components ((:file "str-utils")))
               (:module "tree"
                :pathname "src/tree"
                :components ((:file "tree")))
               (:module "outline"
                :pathname "src/tree"
                :depends-on ("tree")
                :components ((:file "outline")))
               (:module "buffer"
                :pathname "src/buffer"
                :depends-on ("str-utils")
                :components ((:file "region")
                             (:file "change")
                             (:file "buffer")))
               (:module "combinator"
                :pathname "src/combinator"
                :depends-on ("tree" "buffer")
                :components ((:file "reader")
                             (:file "match")
                             (:file "combinator")
                             (:file "transform")))
               (:module "base"
                :pathname "src/base/"
                :depends-on ("combinator" "file-utils" "str-utils" "buffer")
                :components ((:file "base")
                             (:file "utils")
                             (:file "text-object")
                             (:file "link")
                             (:file "text-format")
                             (:file "parser")
                             (:file "convert")))
               (:module "latex-previews"
                :pathname "src/extra/"
                :depends-on ("base" "file-utils" "str-utils")
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
               (:module "babel"
                :pathname "src/babel"
                :depends-on ("tree")
                :components ((:file "babel")
                             (:file "python")))
               ;; TODO: maybe org-mode should not directly depend on roam/agenda/babel.
               (:module "org-mode"
                :pathname "src/format/org-mode"
                :depends-on ("file-utils"
                             "str-utils"
                             "combinator"
                             "latex-previews"
                             "base"
                             "latex"
                             "html"
                             "roam"
                             "agenda")
                :components ((:file "org-mode")
                             (:file "org-list")
                             (:file "org-table")
                             (:file "org-babel")))
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
                :depends-on ("base" "org-mode" "latex-previews")
                :components ((:file "utils")
                             (:file "main")
                             (:file "combinator")
                             (:file "transform")
                             (:file "convert")
                             (:file "incremental")
                             (:file "outline")
                             (:file "org-table")
                             (:file "org-list")
                             (:file "org-mode")
                             (:file "buffer")
                             (:file "region")
                             (:file "manual")
                             (:file "org-babel")))))