(in-package :cltpt)

(defvar *org-mode-text-object-types*)
(defvar *org-mode-inline-text-object-types*)

;; eval-when wouldnt be enough here..
(defmethod asdf:perform :after ((op asdf:load-op) (system (eql (asdf:find-system "cltpt"))))
  ;; we store the classes themselves instead of the symbols so we dont have to
  ;; run find-class (which is really costly) in other places.
  (setf *org-mode-text-object-types*
        (mapcar 'find-class
                '(org-list org-table
                  org-keyword org-header
                  org-link
                  org-block org-drawer
                  display-math inline-math latex-env
                  org-babel-results ;; org-babel-results-colon
                  ;; text-macro
                  )))
  (setf *org-mode-inline-text-object-types*
        (intersection *org-mode-text-object-types*
                      (mapcar 'find-class
                              '(org-link
                                inline-math
                                )))))

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
(defclass org-block-slow (text-object)
  ((rule
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

(defclass org-block (text-object)
  ((rule
    :allocation :class
    :initform
    (list :begin '(:pattern "#+begin_(%w)")
          :begin-to-hash t
          :begin-conditions '(begin-of-line)
          :end '(:pattern "#+end_(%w)")
          :end-conditions '(begin-of-line)
          :end-to-hash t
          ;; we need to make sure the text after begin_ and end_ is the same
          :pair-predicate (lambda (str b-idx e-idx b-end e-end)
                            (let ((begin-str (subseq str b-idx b-end))
                                  (end-str (subseq str e-idx e-end)))
                              (string= (subseq begin-str 8)
                                       (subseq end-str 6)))))))
  (:documentation "org-mode block."))

(defclass org-header (text-object)
  ((rule
    :allocation :class
    :initform '(:text (:pattern "(%C:*) ")
                :text-conditions (begin-of-line))))
  (:documentation "org-mode header."))

(defmethod text-object-init :after ((obj org-header) str1 opening-region closing-region)
  (let* ((count 0)
         (begin (region-begin opening-region))
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
    (loop for ch across (text-object-text obj)
          while (char= ch #\*)
          do (incf count))
    (setf (text-object-property obj :level) count)
    (setf (text-object-property obj :title)
          header-text)
    (setf (region-end (text-object-opening-region obj)) newline-pos)))

(defmethod text-object-export ((obj org-header) backend)
  (case backend
    ('latex
     (let* ((begin-text (format
                         nil
                         "\\~Asection{"
                         (str:join ""
                                   (loop for i from 1 to (text-object-property obj :level)
                                         collect "sub"))))
            (end-text "}")
            (inner-text (text-object-property obj :title))
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
    ('html
     (let* ((begin-text (format
                         nil
                         "<h~A>"
                         (text-object-property obj :level)))
            (end-text (format
                         nil
                         "</h~A>"
                         (text-object-property obj :level)))
            (inner-text (text-object-property obj :title))
            (final-text (concatenate 'string begin-text inner-text end-text))
            (inner-region
              (make-region :begin (length begin-text)
                           :end (- (length final-text) (length end-text)))))
       (list :text final-text
             :escape t
             :escape-region inner-region
             :reparse t
             :reparse-region inner-region
             :recurse t)))))

(defclass org-keyword (text-object)
  ((rule
    :allocation :class
    :initform (list :text '(:pattern "#+(%W-): (%a)")
                    :text-to-hash t
                    :text-conditions '(begin-of-line))))
  (:documentation "org-mode file-level keyword."))

(defclass org-babel-results (text-object)
  ((rule
    :allocation :class
    :initform (list :text '(:string "#+RESULTS:")
                    :text-conditions '(begin-of-line)
                    :text-to-hash t)))
  (:documentation "org-babel evaluation results."))

;; this does what is necessary to actually make the object contain the element after
;; #+results because initially its only matched as a "keyword".
;; (defmethod text-object-init :after ((obj org-babel-results) str1 opening-region closing-region)
;; the output of a src block may be a table, a drawer, a block, or a region of lines
;; preceded by " :".
(defmethod text-object-finalize ((obj org-babel-results))
  "finalize an org-mode block, grabs #+name and other possible keywords."
  (let ((parent (text-object-parent obj))
        (next-sibling (text-object-next-sibling obj))
        (newline-idx))
    (when parent
      (setq newline-idx (position #\newline
                                  (text-object-text (text-object-parent obj))
                                  :start (region-begin (text-object-opening-region obj)))))
    (if (and next-sibling
             (member (type-of next-sibling)
                     (list 'org-drawer 'org-table 'org-block)
                     :test 'string=)
             (< (text-object-fake-line-num-distance obj next-sibling) 2))
        ;; next child is the result of the results (could be drawer or some other org-element)
        ;; we need to make it the child of this object
        (progn
          (setf (text-object-property obj :value) next-sibling)
          (text-object-set-parent next-sibling obj))
        (when parent
          (let ((str-list))
            (loop while newline-idx
                  for current = (1+ newline-idx)
                  while (< (1+ current) (length (text-object-text parent)))
                  for current-beginning = (subseq (text-object-text parent)
                                                  current
                                                  (+ 2 current))
                  while (string= current-beginning ": ")
                  do (setf newline-idx
                           (position #\newline
                                     (text-object-text parent)
                                     :test 'char=
                                     :start current))
                     (push (subseq (text-object-text parent)
                                   (+ 2 current)
                                   (if newline-idx
                                       newline-idx
                                       (length (text-object-text parent))))
                           str-list))
            (setf (text-object-property obj :value)
                  (str:join (string #\newline) str-list))
            (setf (region-end (text-object-opening-region obj))
                  (or newline-idx (+ 2 (length (text-object-text parent))))))))))

(defmethod text-object-export ((obj org-babel-results) backend)
  (let ((results (text-object-property obj :value)))
    (format nil
            ""
            :recurse nil)))

(defclass org-babel-results-colon (text-object)
  ((rule
    :allocation :class
    :initform '(:region (:string ": ")
                :disallow t)))
  (:documentation "org-babel evaluation results."))

(defmethod text-object-export ((obj org-babel-results-colon) backend)
  (let ((results (text-object-property obj :value)))
    (format nil
            ""
            :recurse nil)))

(defclass org-drawer (text-object)
  ((rule
    :allocation :class
    :initform '(:begin (:pattern ":(%w):")
                :begin-to-hash t
                :begin-conditions (end-of-line)
                :end (:pattern (%or (:string ":END:")
                                    (:string ":end:"))))))
  (:documentation "org-mode drawer."))

;; simply dont export drawers (this isnt the correct org-mode behavior tho)
(defmethod text-object-export ((obj org-drawer) backend)
  "")

(defmethod text-object-init :after ((obj org-block) str1 opening-region closing-region)
  ;; grab the "type" of the block
  (let ((result (cl-ppcre:register-groups-bind (type1)
                    ((format nil "#\\+begin_(~A)" *org-symbol-regex*)
                     (region-text opening-region str1))
                  type1)))
    (setf (text-object-property obj :type) result))
  ;; we need to grab the keywords after the #+begin_block statement, which come in the form
  ;; of :keyword value up to the next newline character
  (let* ((begin (region-begin opening-region))
         (newline-pos (+ begin
                         (position (string #\newline)
                                   (subseq str1 begin)
                                   :test 'string=)))
         (space-pos (+ begin (or (position " " (subseq str1 begin) :test 'string=) 0)))
         (line (when (< space-pos newline-pos)
                 (subseq str1 space-pos newline-pos)))
         (entries (unless (str:emptyp line) (parse-keyword-string line))))
    (dolist (entry entries)
      (setf (text-object-property obj (car entry))
            (or (cdr entry) t)))
    (when (< space-pos newline-pos)
      (setf (region-end (text-object-opening-region obj)) newline-pos))))

;; this isnt accurate, it only checks line number with respect to parent's text
(defun text-object-fake-line-num (obj)
  (count #\newline
         (text-object-text (text-object-parent obj))
         :end (region-begin (text-object-opening-region obj))))
(defun text-object-fake-line-num-distance (obj1 obj2)
  (abs (- (text-object-fake-line-num obj1) (text-object-fake-line-num obj2))))

;; one issue si that siblings might not be set properly if there's no parent document (parser was called with `:as-doc nil')
(defmethod text-object-finalize ((obj org-block))
  "finalize an org-mode block, grabs #+name and other possible keywords."
  (loop for sibling = (text-object-prev-sibling obj) while sibling
        do (if (and (typep sibling 'org-keyword)
                    (equal (text-object-fake-line-num-distance obj sibling) 1))
               ;; as long as #+keyword: val precedes the current line by 1 line, we continue
               ;; grabbing keywords.
               (let ((kw (intern (text-object-property sibling :keyword)
                                 "KEYWORD"))
                     (val (text-object-property sibling :value)))
                 (setf (text-object-property obj kw) val)
                 (setf sibling (text-object-prev-sibling sibling)))
               ;; stop
               (setf sibling nil))))

(defmethod text-object-export ((obj org-block) backend)
  (let ((block-type (text-object-property obj :type))
        (is-verbatim))
    (when (string= block-type "src")
      (setf is-verbatim t))
    (cond
      ((member block-type (list "comment" "my_comment") :test 'string=)
       (list :text "" :reparse t))
      ((string= backend 'latex)
       (let* ((begin-tag (format nil "\\begin{~A}" block-type))
              (end-tag (format nil "\\end{~A}" block-type))
              (my-text (concatenate 'string
                                    begin-tag
                                    (text-object-contents obj)
                                    end-tag))
              (inner-region (make-region :begin (length begin-tag)
                                         :end (- (length my-text) (length end-tag)))))
         (when (string= block-type "src")
           (setf block-type "lstlisting"))
         (list :text my-text
               :reparse (not is-verbatim)
               :escape (not is-verbatim)
               :reparse-region inner-region
               :escape-region inner-region)))
      ((string= backend 'html)
       (let* ((open-tag (if (string= block-type "src")
                            (format nil "<pre><code>" block-type)
                            (format nil "<~A>" block-type)))
              (close-tag (if (string= block-type "src")
                             (format nil "</code></pre>" block-type)
                             (format nil "<~A>" block-type)))
              (text (concatenate 'string
                                 open-tag
                                 (text-object-contents obj)
                                 close-tag)))
         (list :text text
               :recurse t
               :reparse-region (make-region :begin (length open-tag)
                                            :end (- (length text) (length close-tag)))
               :reparse t))))))

(defmethod text-object-init :after ((obj org-keyword) str1 opening-region closing-region)
  (let* ((result (cl-ppcre:register-groups-bind (kw val)
                     ((format nil "\\+(~A): (.*)" *org-symbol-regex*)
                      (region-text opening-region str1))
                   (cons kw val)))
         (keyword (car result))
         (value (cdr result)))
    (setf (text-object-property obj :value) value)
    (setf (text-object-property obj :keyword) keyword)))

(defmethod text-object-export ((obj org-keyword) backend)
  ;; was used for debugging
  #+nil
  (format nil
          "keyword: ~A, val: ~A"
          (text-object-property obj :keyword)
          (text-object-property obj :value))
  (format nil
          ""
          :recurse nil))

(defclass org-link (text-object)
  ((rule
    :allocation :class
    :initform '(:text (:pattern (%or (:pattern "[[(%W-):(%E:[])][(%E:[])]]")
                                     (:pattern "[[(%W-)]]")
                                     (:pattern "[[(%W-):(%E:[])]]"))))))
  (:documentation "org-mode link."))

(defmethod text-object-init :after ((obj org-link) str1 opening-region closing-region)
  (let* ((result (cl-ppcre:register-groups-bind
                     (desc dest)
                     ("\\[(\\[.*?\\])*\\[(.*?)\\]\\]" (region-text opening-region str1))
                   (list desc dest)))
         (desc (car result))
         (dest (cadr result)))
    (setf (text-object-property obj :desc) desc)
    (setf (text-object-property obj :dest) dest)))

(defmethod text-object-export ((obj org-link) backend)
  (cond
    ((string= backend 'latex)
     (list :text (format nil "\\ref{~A}" (text-object-property obj :dest))
           :escape nil))
    ((string= backend 'html)
     (format nil "<a href='~A'></a>" (text-object-property obj :dest)))))

(defclass org-list (text-object)
  ((rule
    :allocation :class
    ;; match region of lines beginning with space or hyphen
    :initform '(:region (:pattern (%or (:string "-")
                                       (:pattern "(%C:1234567890).")
                                       (:pattern "(%C:abcdefghijklmnopqrstuv).")))
                :ignore " ")))
  (:documentation "org-mode list."))

(defmethod text-object-init :after ((obj org-list) str1 opening-region closing-region)
  (let ((list1 (org-list-parse (text-object-text obj))))
    (setf (text-object-property obj :list) list1)))

;; used with the results of `org-list-parse'
(defun deep-copy-org-forest (tree)
  (cond ((null tree) nil)
        ((consp tree)
         (cons (deep-copy-org-forest (car tree))
               (deep-copy-org-forest (cdr tree))))
        ((stringp tree)
         (copy-seq tree))
        (t tree)))

(defmethod text-object-export ((obj org-list) backend)
  (let ((my-list (deep-copy-org-forest (text-object-property obj :list)))
        (possible-children-types *org-mode-inline-text-object-types*))
    (mapcar-forest
     my-list
     (lambda (list-entry)
       (let ((list-entry-text (getf list-entry :text)))
         (setf (getf list-entry :text)
               (export-tree (parse list-entry-text
                                   possible-children-types)
                            backend
                            possible-children-types)))))
    (cond
      ((string= backend 'latex)
       (list :text (org-list-to-latex my-list)
             :recurse nil
             :reparse nil
             :escape nil))
      ((string= backend 'html)
       (list :text (org-list-to-html my-list)
             :recurse nil
             :reparse nil
             :escape nil)))))

(defclass org-table (text-object)
  ((rule
    :allocation :class
    :initform '(:region (:string "|")
                :ignore " ")))
  (:documentation "org-mode table."))

(defmethod text-object-init :after ((obj org-table) str1 opening-region closing-region)
  (let ((table1 (org-table-parse (text-object-text obj))))
    (setf (text-object-property obj :table) table1)))

(defmethod text-object-export ((obj org-table) backend)
  (let ((my-table
          (mapcar
           (lambda (row)
             (mapcar
              (lambda (list-entry-text)
                (export-tree
                 (parse list-entry-text
                        *org-mode-inline-text-object-types*)
                 backend
                 *org-mode-inline-text-object-types*))
              row))
           (text-object-property obj :table))))
    (cond
      ((string= backend 'latex)
       (list :text (org-table-to-latex my-table)
             :recurse nil
             :reparse nil
             :escape nil))
      ((string= backend 'html)
       (list :text (org-table-to-html my-table)
             :recurse nil
             :reparse nil
             :escape nil)))))

(defclass org-document (document)
  ()
  (:documentation "org-mode document."))

(defun ensure-latex-previews-generated (org-doc)
  (let ((mylist))
    (map-text-object
     org-doc
     (lambda (obj)
       (when (or (typep obj 'inline-math)
                 (typep obj 'display-math)
                 (typep obj 'latex-env))
         (push (text-object-text obj) mylist))))
    (generate-svgs-for-latex mylist)))

(defmethod text-object-export ((obj org-document) backend)
  (case backend
    ('latex
     (let* ((my-preamble
              (concatenate 'string
                           (generate-latex-preamble "authorhere" "datehere" "titlehere")
                           (string #\newline)
                           "\\begin{document}"
                           (string #\newline)))
            (my-postamble (concatenate 'string
                                       (string #\newline)
                                       "\\end{document}"))
            (my-text (format nil "~A~A~A"
                             my-preamble
                             (text-object-text obj)
                             my-postamble))
            (inner-region (make-region :begin (length my-preamble)
                                       :end (- (length my-text) (length my-postamble)))))
       (list :text my-text
             :reparse t
             :recurse t
             :reparse-region inner-region
             :escape t
             ;; dont escape the commands in the preamble
             :escape-region inner-region)))
    ('html
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
                                  (text-object-text obj)
                                  my-postamble))
            (inner-region (make-region :begin (length my-preamble)
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

(defun parse-org-file (filepath)
  ;; we need to "finalize" the classes to be able to use MOP
  (ensure-finalized)
  (let* ((result (parse (uiop:read-file-string filepath)
                        *org-mode-text-object-types*
                        :as-doc t
                        :doc-type 'org-document)))
    result))

(defun export-org-doc (org-doc backend)
  (export-tree org-doc
               backend
               *org-mode-text-object-types*))

(defun export-org-file (src dest &optional (backend 'latex))
  (with-open-file (f dest
                     :direction :output
                     :if-exists :supersede
                     :if-does-not-exist :create)
    (write-sequence (export-org-doc (parse-org-file src) backend) f)))

;; code for parsing lsits
;; A
(defun org-list-count-leading-spaces (line)
  "return the number of leading space characters in LINE."
  (let ((count 0))
    (loop for ch across line
          while (char= ch #\space)
          do (incf count))
    count))

;; A
(defun org-list-parse-bullet-line (line expected-indent)
  "if LINE (after EXPECTED-INDENT spaces) begins with a valid bullet,
return three values: T, the bullet marker (a string), and the remaining text.
a valid bullet is either a dash followed by a space, or a sequence of digits/letters
followed by a dot (and optionally a following space).
otherwise, return NIL, NIL, NIL."
  (if (/= (org-list-count-leading-spaces line) expected-indent)
      (values nil nil nil)
      (let ((trimmed (subseq line expected-indent)))
        (cond
          ;; dash bullet.
          ((and (> (length trimmed) 1)
                (char= (char trimmed 0) #\-)
                (char= (char trimmed 1) #\space))
           (values t "-" (subseq trimmed 2)))
          ;; number or letter bullet (e.g., "1." or "a.")
          ((or (digit-char-p (char trimmed 0))
               (alpha-char-p (char trimmed 0)))
           (let ((i 0))
             (loop while (and (< i (length trimmed))
                              (or (digit-char-p (char trimmed i))
                                  (alpha-char-p (char trimmed i))))
                   do (incf i))
             (if (and (< i (length trimmed))
                      (char= (char trimmed i) #\.))
                 (progn
                   (incf i) ;; include the dot in the marker
                   (let ((marker (subseq trimmed 0 i)))
                     ;; optionally skip a following space.
                     (when (and (< i (length trimmed))
                                (char= (char trimmed i) #\space))
                       (incf i))
                     (values t marker (subseq trimmed i))))
                 (values nil nil nil))))
          (t (values nil nil nil))))))

;; A
(defun org-list-collect-extra-lines (lines current-indent)
  "recursively collect extra text lines from LINES that are more indented than CURRENT-INDENT.
stop if a line is indented exactly CURRENT-INDENT+3 and is a valid bullet.
returns two values: a list of extra text lines (trimmed) and the remaining lines."
  (if (or (null lines)
          (<= (org-list-count-leading-spaces (first lines)) current-indent))
      (values nil lines)
      (let ((line (first lines)))
        (if (and (= (org-list-count-leading-spaces line) (+ current-indent 3))
                 (multiple-value-bind (child-valid child-marker child-text)
                     (org-list-parse-bullet-line line (+ current-indent 3))
                   child-valid))
            (values nil lines)
            (multiple-value-bind (collected rem)
                (org-list-collect-extra-lines (rest lines) current-indent)
              (values (cons (string-trim " " line) collected) rem))))))

;; A
(defun org-list-parse-one-item (lines current-indent)
  "parse one item from LINES at the given CURRENT-INDENT.
returns two values: the parsed item as a cons cell (where the car is a plist with
keys :marker and :text,and the cdr is a forest of child items in the same format)
and the remaining lines."
  (multiple-value-bind (valid marker text)
      (org-list-parse-bullet-line (first lines) current-indent)
    (unless valid
      (error "expected bullet at indent ~A: ~A" current-indent (first lines)))
    (let ((remaining (rest lines)))
      ;; collect extra lines that are attached to this bullet.
      (multiple-value-bind (extra-lines rem)
          (org-list-collect-extra-lines remaining current-indent)
        (setf remaining rem)
        ;; check for nested children items.
        (let ((children))
          (when (and remaining
                     (>= (org-list-count-leading-spaces (first remaining))
                         (+ current-indent 3)))
            (multiple-value-bind (child-items rem2)
                (org-list-parse-items remaining (+ current-indent 3))
              (setf children child-items)
              (setf remaining rem2)))
          ;; combine the bullet line text with any extra lines.
          (let* ((clean-text (string-trim " " text))
                 (combined-text (if extra-lines
                                    (concatenate 'string
                                                 clean-text
                                                 (string #\newline)
                                                 (str:join (string #\newline) extra-lines))
                                    clean-text))
                 (node (list :marker marker :text combined-text)))
            ;; return a cons cell where car is the node and cdr is the children forest.
            (values (cons node children) remaining)))))))

;; A
(defun org-list-parse-items (lines current-indent)
  "recursively parse LINES at the CURRENT-INDENT level.
returns two values: a list of parsed items and the remaining lines.
each item is a plist with keys :marker, :text, and :children."
  (if (or (null lines)
          (< (org-list-count-leading-spaces (first lines)) current-indent))
      (values nil lines)
      (multiple-value-bind (item rem-lines)
          (org-list-parse-one-item lines current-indent)
        (multiple-value-bind (siblings final-rem)
            (org-list-parse-items rem-lines current-indent)
          (values (cons item siblings) final-rem)))))

;; A
(defun org-list-parse (text)
  "parses an org-mode list from TEXT and returns its nested structure.
each item is a plist with keys:
  :marker   – the bullet marker (e.g. \"-\", \"1.\", \"a.\")
  :text     – the entire text of the element (bullet line plus any attached lines, with newlines)
  :children – a list of nested items (if any)"
  (multiple-value-bind (items rem)
      (org-list-parse-items
       (str:split (string #\newline)
                  (str:trim text)) ;; trim to avoid last \n
       0)
    items))