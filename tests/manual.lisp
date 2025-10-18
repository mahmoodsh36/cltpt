(in-package :cltpt/tests)

;; tests that are meant to be run (and whose output observed) manually

(defun test-org-parse ()
  (cltpt/tree:tree-show
   (parse-file cltpt/org-mode:*org-mode* "test.org")))

(defun test-org-convert ()
  (let ((cltpt/org-mode:*org-enable-macros* t))
    (cltpt/zoo:init)
    (time
     (progn
       ;; (cltpt/base:convert-file
       ;;  (cltpt/base:text-format-by-name "org-mode")
       ;;  (cltpt/base:text-format-by-name "latex")
       ;;  "test3.org"
       ;;  "test.out.tex")
       (cltpt/base:convert-file
        (cltpt/base:text-format-by-name "org-mode")
        (cltpt/base:text-format-by-name "html")
        ;; "/home/mahmooz/brain/notes/1684594232.org"
        ;; "test.org"
        ;; "test2.org"
        "/home/mahmooz/brain/notes/1697750918.org"
        "test.out.html")
       nil))))

(defun test-org-convert-1 ()
  (time
   (progn
     ;; (cltpt/base:convert-file
     ;;  (cltpt/base:text-format-by-name "org-mode")
     ;;  (cltpt/base:text-format-by-name "latex")
     ;;  ;; "/home/mahmooz/brain/notes/1752690904.866355.org"
     ;;  "/tmp/test.org"
     ;;  "/tmp/test.out.tex")
     (cltpt/base:convert-file
      (cltpt/base:text-format-by-name "org-mode")
      (cltpt/base:text-format-by-name "html")
      "/home/mahmooz/brain/notes/1712235129.org"
      ;; "/tmp/test.org"
      "/tmp/test.out.html")
     nil)))

(defun test-agenda-2 ()
  (let* ((rmr (cltpt/roam:from-files
               '((:path ("/home/mahmooz/brain/notes/" "/home/mahmooz/brain/daily/")
                  :glob "*.org"
                  :format "org-mode"))))
         (agenda (cltpt/agenda:from-roamer rmr))
         (begin-ts (local-time:encode-timestamp 0 0 0 0 1 1 2025))
         (end-ts (local-time:encode-timestamp 0 0 0 0 1 5 2025)))
    (cltpt/agenda:render-agenda agenda :begin-ts begin-ts :end-ts end-ts)))

(defun test-agenda-3 ()
  (let* ((rmr (cltpt/roam:from-files
               '((:path ("./test2.org")
                  :glob "*.org"
                  :format "org-mode"))))
         (agenda (cltpt/agenda:from-roamer rmr)))
    (cltpt/agenda:render-agenda agenda)))

(defun roam-test-1 ()
  (time
   (let* ((rmr (cltpt/roam:from-files
                '((:path ("/home/mahmooz/brain/notes/" "/home/mahmooz/brain/daily/")
                   :glob "*.org"
                   :format "org-mode")))))
     (format t
             "found ~A nodes in a total of ~A documents"
             (length (cltpt/roam:roamer-nodes rmr))
             (length
              (remove-if
               (lambda (node)
                 (cltpt/base:text-object-parent (cltpt/roam:node-text-obj node)))
               (cltpt/roam:roamer-nodes rmr)))))))

(defun agenda-test-1 ()
  (let* ((rmr (cltpt/roam:from-files
               '((:path ("/home/mahmooz/brain/daily/")
                  :glob "*.org"
                  :format "org-mode"))))
         (agenda (cltpt/agenda:from-roamer rmr)))
    agenda))

;; currently :dvipng seems to be broken
(defun test-latex-preview-1 ()
  (loop for my-comp in (list :latex :lualatex)
        append (loop for my-img-conv in (list :dvisvgm :dvipng :imagemagick)
                     append (let ((cltpt/latex::*latex-compiler-key* my-comp)
                                  (cltpt/latex::*default-latex-preview-pipeline* my-img-conv))
                              (cltpt/latex::generate-previews-for-latex
                               (list "\\(x=\\somebrokencommand\\)"
                                     "\\(x=yyy\\)"))))))

(defun test-latex-preview-2 ()
  (let ((cltpt/latex::*latex-compiler-key* :latex))
    (cltpt/latex::generate-previews-for-latex
     (list "\\(x=\\somebrokencommand123\\)"))))

(defun roam-convert-test-1 ()
  (time
   (let* ((rmr (cltpt/roam:from-files
                '((:path ("/home/mahmooz/brain/notes/" "/home/mahmooz/brain/daily/")
                   :glob "*.org"
                   :format "org-mode")))))
     (cltpt/roam:convert-all rmr (cltpt/base:text-format-by-name "html") "/tmp/out-%(identity title).html"))))

(defun roam-get-title-by-id (_id)
  (cltpt/roam:node-title
   (cltpt/roam:get-node-by-id
    (getf cltpt/roam:*roam-convert-data* :roamer)
    _id)))

(defun roam-convert-test-2 ()
  (time
   (let* ((rmr (cltpt/roam:from-files
                '((;;:path ("/home/mahmooz/brain/notes/1710536040.org")
                   :path ("/home/mahmooz/work/cltpt/test2.org")
                   :glob "*.org"
                   :format "org-mode")))))
     (cltpt/roam:convert-all
      rmr
      (cltpt/base:text-format-by-name "html")
      "/tmp/out-%(identity title).html"))))