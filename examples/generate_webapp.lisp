(asdf:load-system :cltpt)

;; this example has become outdated

(defun title-to-filename (title)
  "takes a title and tries to make it into a suitable filename."
  (with-output-to-string (out)
    (loop for char across (string-downcase title)
          do (cond ((or (alphanumericp char)
                        (char= char #\-)
                        (char= char #\.))
                    (write-char char out))
                   ((or (char= char #\space)
                        (char= char #\/)
                        (char= char #\\))
                    (write-char #\_ out))))))

;; define it so we can dynamically bind
(defvar *my-metadata* nil)

(defun generate ()
  (let* ((other-head-contents
           "<script src='darkmode.js'></script>
<script src='main.js'></script>")
         (other-preamble-contents
           "<div class='navbar'>
  <a href='/'>home</a>
  <a href='/blog.html'>blog</a>
  <a href='/search.html'>search</a>
  <a href='/about.html'>about</a>
</div>")
         (cltpt/html:*html-static-route* "/")
         (*my-metadata*
           (list :other-head-contents other-head-contents
                 :other-preamble-contents other-preamble-contents))
         (cltpt/html:*html-postamble* "</body></html>")
         (cltpt/html:*html-preamble*
           "<html>
<head>
  <title> %title </title>
  #(getf cl-user::*my-metadata* :other-head-contents)
</head>
<body>
  #(getf cl-user::*my-metadata* :other-preamble-contents)
")
         (rmr (cltpt/roam:from-files
               '((:path ("/home/mahmooz/notes/")
                  :regex ".*\\.org"
                  :format "org-mode")))))
    (cltpt/roam:convert-all rmr (cltpt/base:text-format-by-name "html")
                            "~/blog/%(cl-user::title-to-filename title).html")))

;; (generate)