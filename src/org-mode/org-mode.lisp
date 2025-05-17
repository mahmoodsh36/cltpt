(defpackage :cltpt/org-mode
  (:use :cl :str :cltpt/base :cltpt/latex :ppcre)
  (:import-from
   :cltpt/base :text-object
   :text-macro :text-macro-ref
   :post-lexer-text-macro :post-lexer-text-macro-ref
   :document)
  (:shadowing-import-from :cl-ppcre split)
  (:import-from
   :cltpt/latex
   :display-math :inline-math :latex-env)
  (:export :org-list-parse :org-list-get-bounds :org-header :org-list
           :org-mode))

(in-package :cltpt/org-mode)

(defun make-org-mode ()
  (make-text-format
   "org-mode"
   '(org-list ;; so far the slowest of the bunch
     ;; org-table
     org-keyword org-header
     org-link
     org-block org-drawer
     cltpt/latex:display-math cltpt/latex:inline-math cltpt/latex:latex-env
     ;; org-babel-results org-babel-results-colon
     ;; org-italic ;; doesnt work atm
     org-emph org-inline-code
     cltpt/base:text-macro cltpt/base:text-macro-ref
     cltpt/base:post-lexer-text-macro cltpt/base:post-lexer-text-macro-ref
     )
   'org-document))

;; `text-format' instance
(defvar org-mode (make-org-mode))
;; whether to convert with preamble/postamble, etc
(defvar *org-mode-convert-with-boilerplate* t)
;; for detecting objects inside table cells, lists, etc
(defvar *org-mode-inline-text-object-types*)

(defun org-mode-text-object-types ()
  (cltpt/base:text-format-text-object-types org-mode))

;; eval-when wouldnt be enough here..
(defmethod asdf:perform :after ((op asdf:load-op) (system (eql (asdf:find-system "cltpt"))))
  (setf *org-mode-inline-text-object-types*
        (intersection (org-mode-text-object-types)
                      '(org-link
                        org-emph org-italic org-inline-code
                        cltpt/latex:inline-math
                        cltpt/base:text-macro cltpt/base:text-macro-ref
                        cltpt/base:post-lexer-text-macro cltpt/base:post-lexer-text-macro-ref))))

(defvar *org-symbol-regex* "[a-zA-Z\\-_]+")

;; A
(defun parse-keyword-string (s)
  "given a string S of the form \":kw1 val1 :kw2 :kw3 val3\", returns
a list of (KEY . VALUE) pairs. if a keyword is not followed by a value,
its value is NIL."
  (let ((tokens (str:split " " s))
        (result)
        (current-key nil))
    (dolist (token tokens)
      (if (and (> (length token) 0)
               (char= (char token 0) #\:))
          (progn
            (setf current-key (intern (string-upcase token) :keyword))
            (push (cons current-key nil) result))
          (when current-key
            (setf (cdr (assoc current-key result :test #'eq)) token)
            (setf current-key nil))))
    (nreverse result)))

;; keeping this here for now as an example of a slow text-object definition (it uses :regex)
(defclass org-block-slow (cltpt/base:text-object)
  ((cltpt/base::rule
    :allocation :class
    :initform
    (list :begin (list :regex (format nil "^#\\+begin_~A" *org-symbol-regex*))
          :end (list :regex (format nil "^#\\+end_~A" *org-symbol-regex*))
          ;; we need to make sure the text after begin_ and end_ is the same
          :pair-predicate (lambda (str b-idx e-idx b-end e-end)
                            (let ((begin-str (subseq str b-idx b-end))
                                  (end-str (subseq str e-idx e-end)))
                              (string= (subseq begin-str 8)
                                       (subseq end-str 6)))))))
  (:documentation "org-mode block."))

(defclass org-block (cltpt/base:text-object)
  ((cltpt/base::rule
    :allocation :class
    :initform
    (list :begin "#+begin_%w"
          :begin-to-hash #\#
          :begin-conditions '(cltpt/base:begin-of-line)
          :end "#+end_%w"
          :end-conditions '(cltpt/base:begin-of-line)
          :end-to-hash #\#
          ;; we need to make sure the text after begin_ and end_ is the same
          :pair-predicate (lambda (str b-idx e-idx b-end e-end)
                            (let ((begin-str (subseq str b-idx b-end))
                                  (end-str (subseq str e-idx e-end)))
                              (string= (subseq begin-str 8)
                                       (subseq end-str 6)))))))
  (:documentation "org-mode block."))

(defclass org-header (cltpt/base:text-object)
  ((cltpt/base::rule
    :allocation :class
    :initform '(:text
                (cltpt/base:consec
                 (cltpt/base:atleast-one (cltpt/base:literal "*"))
                 (cltpt/base:atleast-one (cltpt/base:literal " "))
                 (cltpt/base:all-but-newline))
                :text-conditions (cltpt/base:begin-of-line))))
  (:documentation "org-mode header."))

(defmethod cltpt/base:text-object-init :after ((obj org-header) str1 opening-region closing-region)
  (let* ((count 0)
         (begin (cltpt/base:region-begin opening-region))
         (space-pos (position #\space
                              str1
                              :start begin
                              :test 'char=))
         (newline-pos (position #\newline
                                str1
                                :start begin
                                :test 'char=))
         (header-text (when (< space-pos newline-pos)
                        (subseq str1 (1+ space-pos) newline-pos))))
    (loop for ch across (cltpt/base:text-object-text obj)
          while (char= ch #\*)
          do (incf count))
    (setf (cltpt/base:text-object-property obj :level) count)
    (setf (cltpt/base:text-object-property obj :title)
          header-text)
    (setf (cltpt/base:region-end (cltpt/base:text-object-opening-region obj))
          newline-pos)))

(defmethod cltpt/base:text-object-convert ((obj org-header) backend)
  (cltpt/base:pcase backend
    (cltpt/latex:latex
     (let* ((begin-text (format
                         nil
                         "\\~Asection{"
                         (str:join ""
                                   (loop for i from 1 to (text-object-property obj :level)
                                         collect "sub"))))
            (end-text "}")
            (inner-text (cltpt/base:text-object-property obj :title))
            (final-text (concatenate 'string begin-text inner-text end-text))
            (inner-region
              (make-region :begin (length begin-text)
                           :end (- (length final-text) (length end-text)))))
       (list :text final-text
             :escape t
             :escape-region inner-region
             :reparse t
             :reparse-region inner-region
             :recurse t)))
    (cltpt/html:html
     (let* ((begin-text (format
                         nil
                         "<h~A>"
                         (cltpt/base:text-object-property obj :level)))
            (end-text (format
                       nil
                       "</h~A>"
                       (cltpt/base:text-object-property obj :level)))
            (inner-text (cltpt/base:text-object-property obj :title))
            (final-text (concatenate 'string begin-text inner-text end-text))
            (inner-region
              (cltpt/base:make-region :begin (length begin-text)
                                      :end (- (length final-text) (length end-text)))))
       (list :text final-text
             :escape t
             :escape-region inner-region
             :reparse t
             :reparse-region inner-region
             :recurse t)))))

(defclass org-keyword (cltpt/base:text-object)
  ((cltpt/base::rule
    :allocation :class
    :initform (list :text '(cltpt/base:consec "#+%W: %a")
                    :text-to-hash #\#
                    :text-conditions '(cltpt/base:begin-of-line))))
  (:documentation "org-mode file-level keyword."))

(defclass org-babel-results (cltpt/base:text-object)
  ((cltpt/base::rule
    :allocation :class
    :initform (list :text '(cltpt/base:literal-casein "#+results:")
                    :text-conditions '(cltpt/base:begin-of-line)
                    :text-to-hash #\#)))
  (:documentation "org-babel evaluation results."))

;; this does what is necessary to actually make the object contain the element after
;; #+results because initially its only matched as a "keyword".
;; (defmethod text-object-init :after ((obj org-babel-results) str1 opening-region closing-region)
;; the output of a src block may be a table, a drawer, a block, or a region of lines
;; preceded by " :".
(defmethod cltpt/base:text-object-finalize ((obj org-babel-results))
  (let ((parent (cltpt/base:text-object-parent obj))
        (next-sibling (cltpt/base:text-object-next-sibling obj))
        (newline-idx))
    (when parent
      (setf newline-idx (position #\newline
                                  (cltpt/base:text-object-text (cltpt/base:text-object-parent obj))
                                  :start (cltpt/base:region-begin (cltpt/base:text-object-opening-region obj)))))
    (when (and next-sibling
               (member (type-of next-sibling)
                       (list 'org-drawer
                             'org-table
                             'org-block
                             'org-babel-results-colon)
                       :test 'string=)
               (< (text-object-fake-line-num-distance obj next-sibling) 2))
      ;; next child is the result of the results (could be drawer or some other org-element)
      ;; we need to make it the child of this object
      (setf (cltpt/base:text-object-property obj :value) next-sibling)
      ;; (text-object-set-parent next-sibling obj)
      ;; (setf (region-end (text-object-opening-region obj))
      ;;       (max (region-end (text-object-opening-region obj))
      ;;            (region-end (text-object-opening-region next-sibling)))
      ;;       (text-object-text obj)
      ;;       (concatenate 'string
      ;;                    (text-object-text obj)
      ;;                    (string #\newline)
      ;;                    (text-object-text next-sibling))
      ;;       )
      ;; (when parent
      ;;   (let ((str-list))
      ;;     (loop while newline-idx
      ;;           for current = (1+ newline-idx)
      ;;           while (< (1+ current) (length (text-object-text parent)))
      ;;           for current-beginning = (subseq (text-object-text parent)
      ;;                                           current
      ;;                                           (+ 2 current))
      ;;           while (string= current-beginning ": ")
      ;;           do (setf newline-idx
      ;;                    (position #\newline
      ;;                              (text-object-text parent)
      ;;                              :test 'char=
      ;;                              :start current))
      ;;              (push (subseq (text-object-text parent)
      ;;                            (+ 2 current)
      ;;                            (if newline-idx
      ;;                                newline-idx
      ;;                                (length (text-object-text parent))))
      ;;                    str-list))
      ;;     (setf (text-object-property obj :value)
      ;;           (str:join (string #\newline) str-list))
      ;;     (setf (region-end (text-object-opening-region obj))
      ;;           (or newline-idx (+ 2 (length (text-object-text parent)))))))
      )))

(defmethod cltpt/base:text-object-convert ((obj org-babel-results) backend)
  (let ((results (cltpt/base:text-object-property obj :value)))
    (when (typep results 'text-object)
      (setf (cltpt/base:text-object-property results :converts) t))
    ""))

(defclass org-babel-results-colon (cltpt/base:text-object)
  ((cltpt/base::rule
    :allocation :class
    :initform '(:region (cltpt/base:literal ": ")
                :disallow t))))

(defmethod cltpt/base:text-object-convert ((obj org-babel-results-colon) backend)
  (if (cltpt/base:text-object-property obj :converts t)
      (cltpt/base:text-object-text obj)
      ""))

(defun not-drawer-end (str pos match-str)
  (not (string= (string-downcase match-str) ":end:")))

(defclass org-drawer (cltpt/base:text-object)
  ((cltpt/base::rule
    :allocation :class
    :initform '(:begin ":%w:"
                :begin-to-hash #\:
                :begin-conditions (cltpt/base:end-of-line not-drawer-end)
                :end (cltpt/base:literal-casein ":end:"))))
  (:documentation "org-mode drawer."))

;; simply dont convert drawers (this isnt the correct org-mode behavior tho)
(defmethod cltpt/base:text-object-convert ((obj org-drawer) backend)
  "")

(defmethod cltpt/base:text-object-init :after ((obj org-block) str1 opening-region closing-region)
  ;; grab the "type" of the block
  (let ((result (cl-ppcre:register-groups-bind (type1)
                    ((format nil "#\\+begin_(~A)" *org-symbol-regex*)
                     (cltpt/base:region-text opening-region str1))
                  type1)))
    (setf (cltpt/base:text-object-property obj :type) result))
  ;; we need to grab the keywords after the #+begin_block statement, which come in the form
  ;; of :keyword value up to the next newline character
  (let* ((begin (cltpt/base:region-begin opening-region))
         (newline-pos (+ begin
                         (position (string #\newline)
                                   (subseq str1 begin)
                                   :test 'string=)))
         (space-pos (+ begin (or (position " " (subseq str1 begin) :test 'string=) 0)))
         (line (when (< space-pos newline-pos)
                 (subseq str1 space-pos newline-pos)))
         (entries (unless (str:emptyp line) (parse-keyword-string line))))
    (dolist (entry entries)
      (setf (cltpt/base:text-object-property obj (car entry))
            (or (cdr entry) t)))
    (when (< space-pos newline-pos)
      (setf (cltpt/base:region-end (cltpt/base:text-object-opening-region obj)) newline-pos))))

;; this isnt accurate, it only checks line number with respect to parent's text
(defun text-object-fake-line-num (obj)
  (count #\newline
         (cltpt/base:text-object-text (cltpt/base:text-object-parent obj))
         :end (cltpt/base:region-begin (cltpt/base:text-object-opening-region obj))))
(defun text-object-fake-line-num-distance (obj1 obj2)
  (abs (- (text-object-fake-line-num obj1) (text-object-fake-line-num obj2))))

(defmethod cltpt/base:text-object-finalize ((obj org-block))
  "finalize an org-mode block, grabs #+name and other possible keywords."
  (let ((siblilng obj))
    (loop for sibling = (when sibling (cltpt/base:text-object-prev-sibling sibling)) while sibling
          do (if (and (typep sibling 'org-keyword)
                      (equal (text-object-fake-line-num-distance obj sibling) 1))
                 ;; as long as #+keyword: val precedes the current line by 1 line, we continue
                 ;; grabbing keywords.
                 (let ((kw (intern (cltpt/base:text-object-property sibling :keyword)
                                   "KEYWORD"))
                       (val (cltpt/base:text-object-property sibling :value)))
                   (setf (cltpt/base:text-object-property obj kw) val)
                   )
                 ;; stop
                 (setf sibling nil)))))

(defmethod cltpt/base:text-object-convert ((obj org-block) backend)
  (let ((block-type (cltpt/base:text-object-property obj :type))
        (is-verbatim))
    (when (string= block-type "src")
      (setf is-verbatim t))
    (cond
      ((member block-type (list "comment" "my_comment") :test 'string=)
       (list :text "" :reparse t))
      ((eq backend cltpt/latex:latex)
       (let* ((begin-tag (format nil "\\begin{~A}" block-type))
              (end-tag (format nil "\\end{~A}" block-type))
              (my-text (concatenate 'string
                                    begin-tag
                                    (cltpt/base:text-object-contents obj)
                                    end-tag))
              (inner-region (cltpt/base:make-region :begin (length begin-tag)
                                                    :end (- (length my-text) (length end-tag)))))
         (when (string= block-type "src")
           (setf block-type "lstlisting"))
         (list :text my-text
               :reparse (not is-verbatim)
               :escape (not is-verbatim)
               :reparse-region inner-region
               :escape-region inner-region)))
      ((eq backend cltpt/html:html)
       (let* ((open-tag (if (string= block-type "src")
                            (format nil "<pre><code>" block-type)
                            (format nil "<~A>" block-type)))
              (close-tag (if (string= block-type "src")
                             (format nil "</code></pre>" block-type)
                             (format nil "<~A>" block-type)))
              (text (concatenate 'string
                                 open-tag
                                 (cltpt/base:text-object-contents obj)
                                 close-tag)))
         (list :text text
               :recurse t
               :reparse-region (cltpt/base:make-region :begin (length open-tag)
                                                       :end (- (length text) (length close-tag)))
               :reparse t))))))

(defmethod cltpt/base:text-object-init :after ((obj org-keyword) str1 opening-region closing-region)
  (let* ((result (cl-ppcre:register-groups-bind (kw val)
                     ((format nil "\\+(~A): (.*)" *org-symbol-regex*)
                      (region-text opening-region str1))
                   (cons kw val)))
         (keyword (car result))
         (value (cdr result)))
    (setf (cltpt/base:text-object-property obj :value) value)
    (setf (cltpt/base:text-object-property obj :keyword) keyword)))

(defmethod cltpt/base:text-object-convert ((obj org-keyword) backend)
  ;; was used for debugging
  #+nil
  (format nil
          "keyword: ~A, val: ~A"
          (cltpt/base:text-object-property obj :keyword)
          (cltpt/base:text-object-property obj :value))
  (format nil
          ""
          :recurse nil))

(defclass text-link (cltpt/base:text-object)
  ((cltpt/base::rule
    :allocation :class
    :initform `(:pattern
                (cltpt/base:consec
                 (cltpt/base:literal "[[")
                 (:pattern (cltpt/base:literal "][]]")
                  :id link)
                 )
                :end-to-hash t)))
  (:documentation "a link."))

(defclass org-link (cltpt/base:text-object)
  ((cltpt/base::shared-name
    :allocation :class
    :initform 'cltpt/base::link)
   (cltpt/base::rule
    :allocation :class
    :initform '(:text
                (cltpt/base:any
                 ;; [[type:dest][desc]]
                 (cltpt/base:consec
                  "[["
                  (:pattern (cltpt/base:symbol-matcher) :id link-type)
                  ":"
                  (:pattern (cltpt/base:all-but "[]") :id link-dest)
                  "]["
                  (:pattern (cltpt/base:all-but "[]") :id link-desc)
                  "]]")
                 ;; [[dest]]
                 (cltpt/base:consec
                  "[["
                  (:pattern (cltpt/base:all-but "[]") :id link-dest)
                  "]]")
                 ;; [[type:dest]]
                 (cltpt/base:consec
                  "[["
                  (:pattern (cltpt/base:symbol-matcher) :id link-type)
                  ":"
                  (:pattern (cltpt/base:all-but "[]") :id link-dest)
                  "]]")))))
  (:documentation "org-mode link."))

(defmethod cltpt/base:text-object-init :after ((obj org-link) str1 opening-region closing-region)
  (let* ((result (cl-ppcre:register-groups-bind
                     (desc dest)
                     ("\\[(\\[.*?\\])*\\[(.*?)\\]\\]" (cltpt/base:region-text opening-region str1))
                   (list desc dest)))
         (desc (car result))
         (dest (cadr result)))
    (setf (cltpt/base:text-object-property obj :desc) desc)
    (setf (cltpt/base:text-object-property obj :dest) dest)))

(defmethod cltpt/base:text-object-convert ((obj org-link) backend)
  (cond
    ((eq backend cltpt/latex:latex)
     (list :text (format nil "\\ref{~A}" (cltpt/base:text-object-property obj :dest))
           :escape nil))
    ((eq backend cltpt/html:html)
     (format nil "<a href='~A'></a>" (cltpt/base:text-object-property obj :dest)))))

(defun org-list-parser-func (str pos)
  (let ((result (org-list-get-bounds str pos)))
    (when result
      (- (cdr result) (car result)))))

(defclass org-list (cltpt/base:text-object)
  ((cltpt/base::rule
    :allocation :class
    ;; match region of lines beginning with space or hyphen
    :initform '(:text (org-list-parser-func)
                :text-conditions (cltpt/base:begin-of-line))))
  (:documentation "org-mode list."))

(defmethod cltpt/base:text-object-init :after ((obj org-list) str1 opening-region closing-region)
  (let ((list1 (cltpt/base:org-list-parse (cltpt/base:text-object-text obj))))
    (setf (cltpt/base:text-object-property obj :list) list1)))

;; used with the results of `org-list-parse'
(defun deep-copy-org-forest (tree)
  (cond ((null tree) nil)
        ((consp tree)
         (cons (deep-copy-org-forest (car tree))
               (deep-copy-org-forest (cdr tree))))
        ((stringp tree)
         (copy-seq tree))
        (t tree)))

(defmethod cltpt/base:text-object-convert ((obj org-list) backend)
  (let ((my-list (deep-copy-org-forest (cltpt/base:text-object-property obj :list)))
        (possible-children-types *org-mode-inline-text-object-types*))
    ;; (cltpt/base:mapcar-forest
    ;;  my-list
    ;;  (lambda (list-entry)
    ;;    (let ((list-entry-text (getf list-entry :text)))
    ;;      (setf (getf list-entry :text)
    ;;            (cltpt/base:convert-tree (parse list-entry-text
    ;;                                            possible-children-types)
    ;;                                     backend
    ;;                                     possible-children-types)))))
    (cond
      ((eq backend cltpt/latex:latex)
       (list :text (org-list-to-latex my-list)
             :recurse nil
             :reparse nil
             :escape nil))
      ((eq backend cltpt/html:html)
       (list :text (org-list-to-html my-list)
             :recurse nil
             :reparse nil
             :escape nil)))))

(defclass org-table (cltpt/base:text-object)
  ((cltpt/base::rule
    :allocation :class
    :initform '(:region (:string "|")
                :ignore " "
                :disallow t)))
  (:documentation "org-mode table."))

(defmethod cltpt/base:text-object-init :after ((obj org-table) str1 opening-region closing-region)
  (let ((table1 (org-table-parse (cltpt/base:text-object-text obj))))
    (setf (cltpt/base:text-object-property obj :table) table1)))

(defmethod cltpt/base:text-object-convert ((obj org-table) backend)
  (let ((my-table
          (mapcar
           (lambda (row)
             (mapcar
              (lambda (entry-text)
                (cltpt/base:convert-tree
                 (cltpt/base:parse entry-text
                                   *org-mode-inline-text-object-types*)
                 backend
                 *org-mode-inline-text-object-types*))
              row))
           (cltpt/base:text-object-property obj :table))))
    (cond
      ((eq backend cltpt/latex:latex)
       (list :text (org-table-to-latex my-table)
             :recurse nil
             :reparse nil
             :escape nil))
      ((eq backend cltpt/html:html)
       (list :text (org-table-to-html my-table)
             :recurse nil
             :reparse nil
             :escape nil)))))

(defclass org-document (cltpt/base:document)
  ()
  (:documentation "org-mode document."))

(defun ensure-latex-previews-generated (org-doc)
  (let ((mylist))
    (cltpt/base:map-text-object
     org-doc
     (lambda (obj)
       (when (or (typep obj 'cltpt/latex:inline-math)
                 (typep obj 'cltpt/latex:display-math)
                 (typep obj 'cltpt/latex:latex-env))
         (push (cltpt/base:text-object-text obj) mylist))))
    (generate-svgs-for-latex mylist)))

(defmethod cltpt/base:text-object-convert ((obj org-document) backend)
  (cltpt/base:pcase backend
    (cltpt/latex:latex
     (let* ((my-preamble
              (if *org-mode-convert-with-boilerplate*
                  (concatenate 'string
                               (cltpt/latex:generate-latex-preamble "authorhere" "datehere" "titlehere")
                               (string #\newline)
                               "\\begin{document}"
                               (string #\newline))
                  ""))
            (my-postamble
              (if *org-mode-convert-with-boilerplate*
                  (concatenate 'string
                               (string #\newline)
                               "\\end{document}")
                  ""))
            (my-text (format nil "~A~A~A"
                             my-preamble
                             (cltpt/base:text-object-text obj)
                             my-postamble))
            (inner-region (cltpt/base:make-region :begin (length my-preamble)
                                                  :end (- (length my-text) (length my-postamble)))))
       (list :text my-text
             :reparse t
             :recurse t
             :reparse-region inner-region
             :escape t
             ;; dont escape the commands in the preamble
             :escape-region inner-region)))
    (cltpt/html:html
     (let* ((my-preamble
              (concatenate 'string
                           "<html>"
                           (string #\newline)
                           (generate-html-header "authorhere" "datehere" "titlehere")
                           (string #\newline)
                           "<body><div id='content'>"
                           (string #\newline)))
            (my-postamble (concatenate 'string
                                       (string #\newline)
                                       "</div></body></html>"))
            (my-text (concatenate 'string
                                  my-preamble
                                  (cltpt/base:text-object-text obj)
                                  my-postamble))
            (inner-region (cltpt/base:make-region :begin (length my-preamble)
                                                  :end (- (length my-text) (length my-postamble)))))
       (ensure-latex-previews-generated obj)
       (list :text my-text
             :reparse t
             :recurse t
             :reparse-region inner-region
             :escape t
             :escape-region inner-region)))))

;; org-table parser
;; A
(defun org-table-separator-line-p (line)
  (and line (search "+" line)))
(defun split-table-line (line)
  (let* ((cells (uiop:split-string line :separator (string #\|)))
         (cells (if (and cells (string= (first cells) ""))
                    (rest cells)
                    cells))
         (cells (if (and cells (string= (car (last cells)) ""))
                    (butlast cells)
                    cells)))
    (mapcar (lambda (cell) (string-trim " " cell)) cells)))
(defun org-table-parse (text)
  (let* ((lines (uiop:split-string text :separator (string #\newline)))
         (rows (remove-if (lambda (line)
                            (or (string= line "")
                                (org-table-separator-line-p line)))
                          lines)))
    (mapcar #'split-table-line rows)))

(defclass org-emph (cltpt/base:text-object)
  ((cltpt/base::rule
    :allocation :class
    :initform `(:begin (cltpt/base:literal "*")
                :begin-to-hash #\*
                :begin-conditions ,(list (complement #'cltpt/base:begin-of-line))
                :end-conditions ,(list (complement #'cltpt/base:begin-of-line))
                :end (cltpt/base:literal "*")
                :end-to-hash #\*
                :nestable nil
                :disallow t
                :same-line t)))
  (:documentation "org-mode emphasized text (surrounded by stars)."))

(defmethod cltpt/base:text-object-convert ((obj org-emph) backend)
  (cond
    ((eq backend cltpt/latex:latex)
     (let ((result (cltpt/base:wrap-contents-for-convert obj "\\textbf{" "}")))
       (setf (getf result :reparse-region) nil)
       result))
    ((eq backend cltpt/html:html)
     (cltpt/base:wrap-contents-for-convert obj "<b>" "</b>"))))

(defclass org-italic (cltpt/base:text-object)
  ((cltpt/base::rule
    :allocation :class
    :initform '(:begin (cltpt/base:literal "/")
                :begin-to-hash #\/
                :end (cltpt/base:literal "/")
                :end-to-hash #\/
                :disallow t
                :nestable nil
                :same-line t)))
  (:documentation "org-mode italicized text (surrounded by forward slahes)."))

(defmethod cltpt/base:text-object-convert ((obj org-italic) backend)
  (cond
    ((eq backend cltpt/latex:latex)
     (let ((result (cltpt/base:wrap-contents-for-convert obj "\\textit{" "}")))
       (setf (getf result :reparse-region) nil)
       result))
    ((eq backend cltpt/html:html)
     (cltpt/base:wrap-contents-for-convert obj "<i>" "</i>"))))

(defclass org-inline-code (cltpt/base:text-object)
  ((cltpt/base::rule
    :allocation :class
    :initform '(:begin (cltpt/base:literal "~")
                :begin-to-hash #\~
                :end (cltpt/base:literal "~")
                :end-to-hash #\~
                :disallow t
                :nestable nil
                :same-line t)))
  (:documentation "org-mode inline code (surrounded by tildes)."))

(defmethod cltpt/base:text-object-convert ((obj org-inline-code) backend)
  (cond
    ((eq backend cltpt/latex:latex)
     (let ((result (cltpt/base:wrap-contents-for-convert obj "\\verb{" "}")))
       (setf (getf result :reparse-region) nil)
       result))
    ((eq backend cltpt/html:html)
     (cltpt/base:wrap-contents-for-convert obj "<pre><code>" "</code></pre>"))))

(defun parse-org-file (filepath)
  ;; we need to "finalize" the classes to be able to use MOP
  (let* ((result (parse (uiop:read-file-string filepath)
                        (org-mode-text-object-types)
                        :doc-type 'org-document)))
    result))

(defun convert-org-doc (org-doc backend)
  (cltpt/base:convert-tree org-doc
                           backend
                           (org-mode-text-object-types)))

(defun convert-org-file (src dest &optional (backend 'latex))
  (with-open-file (f dest
                     :direction :output
                     :if-exists :supersede
                     :if-does-not-exist :create)
    (write-sequence (convert-org-doc (parse-org-file src) backend) f)))