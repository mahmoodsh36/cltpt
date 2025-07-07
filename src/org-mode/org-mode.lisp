(defpackage :cltpt/org-mode
  (:use :cl :str :cltpt/base :cltpt/latex)
  (:import-from
   :cltpt/base :text-object
   :text-macro :post-lexer-text-macro
   :document)
  (:import-from
   :cltpt/latex
   :display-math :inline-math :latex-env)
  (:export :org-list-matcher :org-header :org-list
           :org-mode))

(in-package :cltpt/org-mode)

(defun make-org-mode ()
  (make-text-format
   "org-mode"
   '(org-list
     org-table
     org-keyword
     org-header
     org-link
     org-block
     org-drawer
     cltpt/latex:display-math cltpt/latex:inline-math cltpt/latex:latex-env
     ;; org-babel-results org-babel-results-colon
     org-italic
     org-emph
     org-inline-code
     cltpt/base:text-macro
     cltpt/base:post-lexer-text-macro
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
                        cltpt/base:text-macro
                        cltpt/base:post-lexer-text-macro))))

;; (defun parse-keyword-string (s)
;;   "given a string S of the form \":kw1 val1 :kw2 :kw3 val3\", returns
;; a list of (KEY . VALUE) pairs. if a keyword is not followed by a value,
;; its value is NIL."
;;   (let ((tokens (str:split " " s))
;;         (result)
;;         (current-key nil))
;;     (dolist (token tokens)
;;       (if (and (> (length token) 0)
;;                (char= (char token 0) #\:))
;;           (progn
;;             (setf current-key (intern (string-upcase token) :keyword))
;;             (push (cons current-key nil) result))
;;           (when current-key
;;             (setf (cdr (assoc current-key result :test #'eq)) token)
;;             (setf current-key nil))))
;;     (nreverse result)))

(defvar *org-keyword-rule*
  '(cltpt/combinator:consec
    (cltpt/combinator:literal "#+")
    (:pattern (cltpt/combinator:symbol-matcher)
     :id keyword)
    (cltpt/combinator:literal ":")
    (cltpt/combinator:atleast-one (cltpt/combinator:literal " "))
    (:pattern (cltpt/combinator:all-but-newline)
     :id value)))
(defclass org-keyword (cltpt/base:text-object)
  ((cltpt/base::rule
    :allocation :class
    :initform *org-keyword-rule*))
  (:documentation "org-mode file-level keyword."))

(defvar *org-block-no-kw-rule*
  '(cltpt/combinator:pair
    (cltpt/combinator:unescaped
     (:pattern
      (cltpt/combinator:any
       (cltpt/combinator:consec
        (cltpt/combinator:literal "#+begin_")
        (:pattern (cltpt/combinator:symbol-matcher)
         :id begin-type)
        (:pattern (cltpt/combinator:all-but-newline)
         :id keywords))
       (cltpt/combinator:consec
        (cltpt/combinator:literal "#+begin_")
        (:pattern (cltpt/combinator:symbol-matcher)
         :id begin-type)))
      :id begin))
    (cltpt/combinator:unescaped
     (:pattern
      (cltpt/combinator:consec
       (cltpt/combinator:literal "#+end_")
       (:pattern (cltpt/combinator:symbol-matcher)
        :id end-type))
      :id end))))
(defvar *org-block-rule*
  `(cltpt/combinator:any
    (cltpt/combinator:consec
     ,*org-keyword-rule*
     ,*org-block-no-kw-rule*)
    ,*org-block-no-kw-rule*))
(defclass org-block (cltpt/base:text-object)
  ((cltpt/base::rule
    :allocation :class
    :initform *org-block-rule*))
  (:documentation "org-mode block."))

(defvar *org-header-rule*
  '(cltpt/combinator:any
    (cltpt/combinator:consec
     (:pattern (cltpt/combinator:atleast-one (cltpt/combinator:literal "*"))
      :id stars)
     (:pattern (cltpt/combinator:upcase-word-matcher)
      :id todo-keyword)
     (cltpt/combinator:atleast-one (cltpt/combinator:literal " "))
     (:pattern (cltpt/combinator:all-but-newline)
      :id title))
    (cltpt/combinator:consec
     (:pattern (cltpt/combinator:atleast-one (cltpt/combinator:literal "*"))
      :id stars)
     (cltpt/combinator:atleast-one (cltpt/combinator:literal " "))
     (:pattern (cltpt/combinator:all-but-newline)
      :id title))))
(defclass org-header (cltpt/base:text-object)
  ((cltpt/base::rule
    :allocation :class
    :initform *org-header-rule*))
  (:documentation "org-mode header."))

(defmethod cltpt/base:text-object-init :after ((obj org-header) str1 match)
  (let* ((stars (cltpt/base::find-submatch match 'stars))
         (title (cltpt/base::find-submatch match 'title))
         (todo-keyword (cltpt/base::find-submatch match 'todo-keyword))
         (scheduled (encode-universal-time 0 0 10 10 7 2025 0)))
    (setf (cltpt/base:text-object-property obj :level)
          (length (getf stars :match)))
    (setf (cltpt/base:text-object-property obj :title)
          (getf title :match))
    (setf (cltpt/base:text-object-property obj :todo)
          (cltpt/agenda:make-todo
           :title "test1"
           :description "test2"
           :scheduled scheduled
           :deadline nil
           :tags nil
           :children nil))
    ))

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

(defclass org-babel-results (cltpt/base:text-object)
  ((cltpt/base::rule
    :allocation :class
    :initform (list '(cltpt/combinator:literal-casein "#+results:"))))
  (:documentation "org-babel evaluation results."))

;; this does what is necessary to actually make the object contain the element after
;; #+results because initially its only matched as a "keyword".
;; (defmethod text-object-init :after ((obj org-babel-results) str1 opening-region closing-region)
;; the output of a src block may be a table, a drawer, a block, or a region of lines
;; preceded by " :".
;; (defmethod cltpt/base:text-object-finalize ((obj org-babel-results))
;;   (let ((parent (cltpt/base:text-object-parent obj))
;;         (next-sibling (cltpt/base:text-object-next-sibling obj))
;;         (newline-idx))
;;     (when parent
;;       (setf newline-idx (position #\newline
;;                                   (cltpt/base:text-object-text (cltpt/base:text-object-parent obj))
;;                                   :start (cltpt/base:region-begin (cltpt/base:text-object-opening-region obj)))))
;;     (when (and next-sibling
;;                (member (type-of next-sibling)
;;                        (list 'org-drawer
;;                              'org-table
;;                              'org-block
;;                              'org-babel-results-colon)
;;                        :test 'string=)
;;                (< (text-object-fake-line-num-distance obj next-sibling) 2))
;;       ;; next child is the result of the results (could be drawer or some other org-element)
;;       ;; we need to make it the child of this object
;;       (setf (cltpt/base:text-object-property obj :value) next-sibling)
;;       ;; (text-object-set-parent next-sibling obj)
;;       ;; (setf (region-end (text-object-opening-region obj))
;;       ;;       (max (region-end (text-object-opening-region obj))
;;       ;;            (region-end (text-object-opening-region next-sibling)))
;;       ;;       (text-object-text obj)
;;       ;;       (concatenate 'string
;;       ;;                    (text-object-text obj)
;;       ;;                    (string #\newline)
;;       ;;                    (text-object-text next-sibling))
;;       ;;       )
;;       ;; (when parent
;;       ;;   (let ((str-list))
;;       ;;     (loop while newline-idx
;;       ;;           for current = (1+ newline-idx)
;;       ;;           while (< (1+ current) (length (text-object-text parent)))
;;       ;;           for current-beginning = (subseq (text-object-text parent)
;;       ;;                                           current
;;       ;;                                           (+ 2 current))
;;       ;;           while (string= current-beginning ": ")
;;       ;;           do (setf newline-idx
;;       ;;                    (position #\newline
;;       ;;                              (text-object-text parent)
;;       ;;                              :test 'char=
;;       ;;                              :start current))
;;       ;;              (push (subseq (text-object-text parent)
;;       ;;                            (+ 2 current)
;;       ;;                            (if newline-idx
;;       ;;                                newline-idx
;;       ;;                                (length (text-object-text parent))))
;;       ;;                    str-list))
;;       ;;     (setf (text-object-property obj :value)
;;       ;;           (str:join (string #\newline) str-list))
;;       ;;     (setf (region-end (text-object-opening-region obj))
;;       ;;           (or newline-idx (+ 2 (length (text-object-text parent)))))))
;;       )))

(defmethod cltpt/base:text-object-convert ((obj org-babel-results) backend)
  (let ((results (cltpt/base:text-object-property obj :value)))
    (when (typep results 'text-object)
      (setf (cltpt/base:text-object-property results :converts) t))
    ""))

(defclass org-babel-results-colon (cltpt/base:text-object)
  ((cltpt/base::rule
    :allocation :class
    :initform '(:region (cltpt/combinator:literal ": ")
                :disallow t))))

(defmethod cltpt/base:text-object-convert ((obj org-babel-results-colon) backend)
  (if (cltpt/base:text-object-property obj :converts t)
      (cltpt/base:text-object-text obj)
      ""))

(defclass org-drawer (cltpt/base:text-object)
  ((cltpt/base::rule
    :allocation :class
    :initform '(cltpt/combinator::pair
                (cltpt/combinator::followed-by
                 ":%w:"
                 cltpt/combinator::at-line-end-p)
                (cltpt/combinator::when-match
                 (cltpt/combinator:literal-casein ":end:")
                 cltpt/combinator::at-line-start-p))))
  (:documentation "org-mode drawer."))

;; simply dont convert drawers (this isnt the correct org-mode behavior tho)
(defmethod cltpt/base:text-object-convert ((obj org-drawer) backend)
  "")

(defmethod cltpt/base:text-object-init :after ((obj org-block) str1 match)
  ;; grab the "type" of the block, set content boundaries, need to grab keywords
  (let* ((begin-type-match (cltpt/base:find-submatch match 'begin-type))
         (begin-type (getf begin-type-match :match))
         (begin-match (cltpt/base:find-submatch match 'begin))
         (end-match (cltpt/base:find-submatch match 'end))
         (keywords-str (cltpt/base:find-submatch match 'keywords)))
    (setf (cltpt/base:text-object-property obj :type) begin-type)
    (setf (cltpt/base:text-object-property obj :contents-region)
          (cltpt/base:make-region :begin (- (getf begin-match :end)
                                            (getf begin-match :begin))
                                  :end (- (getf end-match :begin)
                                          (getf begin-match :begin))))))

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

(defmethod cltpt/base:text-object-init :after ((obj org-keyword) str1 match)
  (let* ((value-match
           (cltpt/base:tree-find match
                                  'value
                                  :key (lambda (node)
                                         (getf node :id))))
         (keyword-match
           (cltpt/base:tree-find match
                                  'keyword
                                  :key (lambda (node)
                                         (getf node :id)))))
    (setf (cltpt/base:text-object-property obj :value)
          (getf value-match :match))
    (setf (cltpt/base:text-object-property obj :keyword)
          (getf keyword-match :match))))

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
                (cltpt/combinator:consec
                 (cltpt/combinator:literal "[[")
                 (:pattern (cltpt/combinator:literal "][]]")
                  :id link)))))
  (:documentation "a link."))

(defvar *org-link-rule*
  '(cltpt/combinator:any
    ;; [[type:dest][desc]]
    (cltpt/combinator:consec
     "[["
     (:pattern (cltpt/combinator:symbol-matcher) :id link-type)
     ":"
     (:pattern (cltpt/combinator:all-but "[]") :id link-dest)
     "]["
     (:pattern (cltpt/combinator:all-but "[]") :id link-desc)
     "]]")
    ;; [[dest]]
    (cltpt/combinator:consec
     "[["
     (:pattern (cltpt/combinator:all-but "[]") :id link-dest)
     "]]")
    ;; [[type:dest]]
    (cltpt/combinator:consec
     "[["
     (:pattern (cltpt/combinator:symbol-matcher) :id link-type)
     ":"
     (:pattern (cltpt/combinator:all-but "[]") :id link-dest)
     "]]")))
(defclass org-link (cltpt/base:text-object)
  ((cltpt/base::shared-name
    :allocation :class
    :initform 'cltpt/base::link)
   (cltpt/base::rule
    :allocation :class
    :initform *org-link-rule*))
  (:documentation "org-mode link."))

(defmethod cltpt/base:text-object-init :after ((obj org-link) str1 match)
  (let ((link-type-match
          (cltpt/base:tree-find match
                                 'link-type
                                 :key (lambda (node)
                                        (getf node :id))))
        (link-dest-match
          (cltpt/base:tree-find match
                                 'link-dest
                                 :key (lambda (node)
                                        (getf node :id))))
        (link-desc-match
          (cltpt/base:tree-find match
                                 'link-desc
                                 :key (lambda (node)
                                        (getf node :id)))))
    (setf (cltpt/base:text-object-property obj :desc)
          (getf link-desc-match :match))
    (setf (cltpt/base:text-object-property obj :dest)
          (getf link-dest-match :match))
    (setf (cltpt/base:text-object-property obj :type)
          (getf link-type-match :match))))

(defclass org-list (cltpt/base:text-object)
  ((cltpt/base::rule
    :allocation :class
    :initform `(org-list-matcher
                ((:pattern ,*org-link-rule* :id org-link)
                 (:pattern ,cltpt/latex::*inline-math-rule*
                  :id cltpt/latex::inline-math)))))
  (:documentation "org-mode list."))

(defun deep-copy-org-forest (tree)
  (cond ((null tree) nil)
        ((consp tree)
         (cons (deep-copy-org-forest (car tree))
               (deep-copy-org-forest (cdr tree))))
        ((stringp tree)
         (copy-seq tree))
        (t tree)))

;; this is very hacky, perhaps we should find a better way to export lists
;; and their children
(defmethod cltpt/base:text-object-convert ((obj org-list) backend)
  ;; create a new non-org-list object, export it, use the output as a new list
  ;; reparse, that new list, then export the modified newly parsed list as if
  ;; it was the original
  (let ((new-obj (make-instance 'cltpt/base:text-object)))
    (cltpt/base:text-object-init
     new-obj
     (cltpt/base:text-object-text obj)
     (org-list-matcher (cltpt/base:text-object-text obj) 0))
    ;; set children of new-obj to those of obj without any nested org-lists
    ;; otherwise things wont work properly (because nested org-lists get converted)
    ;; to latex and later we try to parse them as a list
    (setf (cltpt/base:text-object-children new-obj)
          (remove-if
           (lambda (obj)
             (equal (type-of obj) 'org-list))
           (cltpt/base:text-object-children obj)))
    (let* ((new-txt (cltpt/base:convert-tree new-obj
                                             backend
                                             (org-mode-text-object-types)))
           (parsed (org-list-matcher new-txt 0)))
      (cond
        ((eq backend cltpt/latex:latex)
         (list :text (to-latex-list parsed)
               :recurse nil
               :reparse nil
               :escape nil))
        ((eq backend cltpt/html:html)
         (list :text (to-html-list my-list)
               :recurse nil
               :reparse nil
               :escape nil))))))

(defclass org-table (cltpt/base:text-object)
  ((cltpt/base::rule
    :allocation :class
    :initform `(org-table-matcher
                ((:pattern ,*org-link-rule* :id org-link)
                 (:pattern ,cltpt/latex:*inline-math-rule*
                  :id cltpt/latex:inline-math)))))
  (:documentation "org-mode table."))

;; hacky, like the one for org-list, they need to be improved
(defmethod cltpt/base:text-object-convert ((obj org-table) backend)
  (let ((new-obj (make-instance 'cltpt/base:text-object)))
    (cltpt/base:text-object-init
     new-obj
     (cltpt/base:text-object-text obj)
     (org-table-matcher (cltpt/base:text-object-text obj) 0))
    (setf (cltpt/base:text-object-children new-obj)
          (cltpt/base:text-object-children obj))
    (let* ((new-txt (cltpt/base:convert-tree
                     new-obj
                     backend
                     (list 'cltpt/latex:inline-math)))
           (parsed (org-table-matcher new-txt 0)))
      (cond
        ((eq backend cltpt/latex:latex)
         (list :text (to-latex-table parsed)
               :recurse nil
               :reparse nil
               :escape nil))
        ((eq backend cltpt/html:html)
         (list :text (to-html-table parsed)
               :recurse nil
               :reparse nil
               :escape nil))))))

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

(defun generate-html-header (author date title)
  "<head></head>")

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

(defclass org-emph (cltpt/base:text-object)
  ((cltpt/base::rule
    :allocation :class
    :initform `(:pattern
                (cltpt/combinator:pair
                 (cltpt/combinator:unescaped (cltpt/combinator:literal "*"))
                 (cltpt/combinator:unescaped (cltpt/combinator:literal "*"))
                 nil
                 nil
                 nil)
                :escapable #\*)))
  (:documentation "org-mode emphasized text (surrounded by asterisks)."))

(defun compress-contents-region-by-one (obj)
  (setf (cltpt/base:text-object-property obj :contents-region)
        (cltpt/base:make-region
         :begin 1
         :end (1-
               (cltpt/base:region-length
                (cltpt/base:text-object-text-region obj))))))

(defmethod cltpt/base:text-object-finalize ((obj org-emph))
  (compress-contents-region-by-one obj))

(defmethod cltpt/base:text-object-convert ((obj org-emph) backend)
  (cond
    ((eq backend cltpt/latex:latex)
     (let ((result (cltpt/base:wrap-contents-for-convert obj "\\textbf{" "}")))
       (setf (getf result :reparse-region) nil)
       result))
    ((eq backend cltpt/html:html)
     (cltpt/base:wrap-contents-for-convert obj "<b>" "</b>"))))

(defvar *org-italic-rule*
  '(cltpt/combinator:pair
    (cltpt/combinator:unescaped (cltpt/combinator:literal "/"))
    (cltpt/combinator:unescaped (cltpt/combinator:literal "/"))
    nil
    nil
    nil))
(defclass org-italic (cltpt/base:text-object)
  ((cltpt/base::rule
    :allocation :class
    :initform *org-italic-rule*))
  (:documentation "org-mode italicized text (surrounded by forward slahes)."))

(defmethod cltpt/base:text-object-finalize ((obj org-italic))
  (compress-contents-region-by-one obj))

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
    :initform
    '(:pattern
      (cltpt/combinator:pair
       (cltpt/combinator:unescaped (cltpt/combinator:literal "~"))
       (cltpt/combinator:unescaped (cltpt/combinator:literal "~"))
       nil nil nil)
      :escapable #\~)))
  (:documentation "org-mode inline code (surrounded by tildes)."))

(defmethod cltpt/base:text-object-finalize ((obj org-inline-code))
  (compress-contents-region-by-one obj))

(defmethod cltpt/base:text-object-convert ((obj org-inline-code) backend)
  (cond
    ((eq backend cltpt/latex:latex)
     (let ((result (cltpt/base:wrap-contents-for-convert obj "\\verb{" "}")))
       (setf (getf result :reparse-region) nil)
       result))
    ((eq backend cltpt/html:html)
     (cltpt/base:wrap-contents-for-convert obj "<pre><code>" "</code></pre>"))))

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