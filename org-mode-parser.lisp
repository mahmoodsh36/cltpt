(in-package :cltpt)

(defvar *org-mode-text-object-types*)

(defmethod asdf:perform :after ((op asdf:load-op) (system (eql (asdf:find-system "cltpt"))))
  ;; eval-when wouldnt be enough here..
  (setf *org-mode-text-object-types*
        (mapcar 'find-class
                '(org-header
                  org-list ;; org-table
                  org-keyword
                  org-link
                  org-block org-drawer
                  display-math inline-math
                  latex-env
                  ))))

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

(defclass org-block-slow (text-object)
  ((rule
    :allocation :class
    :initform
    (list :method 'line-pair
          :data (list :begin (list :regex (format nil "^#\\+begin_~A" *org-symbol-regex*))
                      :end (list :regex (format nil "^#\\+end_~A" *org-symbol-regex*))
                      ;; we need to make sure the text after begin_ and end_ is the same
                      :predicate (lambda (b e)
                                   (string= (subseq b 8) (subseq e 6)))))))
  (:documentation "org-mode block."))

(defclass org-block (text-object)
  ((rule
    :allocation :class
    :initform
    (list :data (list :begin '(:pattern "#+begin_(%w)")
                      :begin-conditions '(begin-of-line)
                      :end '(:pattern "#+end_(%w)")
                      :end-conditions '(begin-of-line)
                      ;; we need to make sure the text after begin_ and end_ is the same
                      :predicate (lambda (b e)
                                   (string= (subseq b 8) (subseq e 6)))))))
  (:documentation "org-mode block."))

(defclass org-header (text-object)
  ((rule
    :allocation :class
    :initform '(:data (:text (:pattern "(%C:*) ")
                       :conditions '(begin-of-line)))))
  (:documentation "org-mode header."))

(defmethod text-object-init :after ((obj org-header) str1 opening-region closing-region)
  (let ((count 0))
    (loop for ch across (text-object-text obj)
          while (char= ch #\*)
          do (incf count))
    (setf (text-object-property obj :level) count)
    (setf (text-object-property obj :title)
          (subseq (text-object-contents obj) (1+ count)))))

(defmethod text-object-export ((obj org-header) backend)
  (case backend
    ('latex
     (list :text (format
                  nil
                  "\\~Asection{~A}"
                  (str:join "" (loop for i from 1 to (text-object-property obj :level) collect "sub"))
                  (text-object-property obj :title))
           :escape nil
           :reparse nil
           :recurse nil))
    ('html
     "")))

(defclass org-keyword (text-object)
  ((rule
    :allocation :class
    :initform (list :data (list :text '(:pattern "#+(%W-): (%a)")
                                :conditions '(begin-of-line)))))
  (:documentation "org-mode file-level keyword."))

;; i need to think of a good way to do this.
;; org-babel results could contain a drawer which itself contains newlines. if we simply look
;; for text started by #+results and ended by a newline it wont be sufficient. actually
;; sometimes #+results dont end with a newline but with a new element (header, block, etc..)
(defclass org-babel-results (text-object)
  ((rule
    :allocation :class
    :initform '(:method custom
                :data 'extract-babel-results-blocks)))
  (:documentation "org-babel evaluation results."))

(defclass org-drawer (text-object)
  ((rule
    :allocation :class
    :initform (list :data '(:begin (:pattern ":(%w):")
                            :begin-conditions (end-of-line)
                            :end (:pattern ":END:"))))) ;; we need to handle lowercase :end:
  (:documentation "org-mode drawer."))

;; simply dont export drawers
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
      (setf is-verbatim t)
      (setf block-type "lstlisting"))
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
         (list :text my-text
               :reparse (not is-verbatim)
               :escape (not is-verbatim)
               :reparse-region inner-region
               :escape-region inner-region)))
      ((string= backend 'html)
       (let* ((open-tag (format nil "<~A>" block-type))
              (close-tag (format nil "</~A>" block-type))
              (text (format nil
                            "~A~A~A"
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
    :initform '(:data (:text (:pattern "[[(%W-):(%W-)][(%C:abcdefghijklmnopqrstuvwxyz )]]")))))
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
    :initform '(:data (:region (:string "-")
                       :ignore " "))))
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

;; we need to fix this
(defmethod text-object-export ((obj org-list) backend)
  (cond
    ((string= backend 'latex)
     (let ((my-list (deep-copy-org-forest (text-object-property obj :list))))
       (mapcar-forest
        my-list
        (lambda (list-entry)
          (let ((list-entry-text (getf list-entry :text)))
            (setf (getf list-entry :text)
                  (export-tree (parse list-entry-text
                                      *org-mode-text-object-types*)
                               'latex
                               *org-mode-text-object-types*)))))
       (list :text (org-list-to-latex my-list)
             :recurse nil
             :reparse nil
             :escape nil)))
    ((string= backend 'html)
     (org-list-to-html (text-object-property obj :list)))))

;; we need to fix this
(defclass org-table (text-object)
  ((rule
    :allocation :class
    :initform '(:method line-region
                :data (:regex "^\\s*[\\|\\+].*[\\|\\+]$"))))
  (:documentation "org-mode table."))

(defmethod text-object-init :after ((obj org-table) str1 opening-region closing-region)
  (let ((table1 (org-table-parse (text-object-text obj))))
    (setf (text-object-property obj :table) table1)))

(defmethod text-object-export ((obj org-table) backend)
  (cond
    ((string= backend 'latex)
     (let ((my-list (text-object-property obj :table)))
       (setf my-list
             (mapcar
              (lambda (row)
                (mapcar
                 (lambda (list-entry-text)
                   (export-tree (parse list-entry-text
                                       *org-mode-text-object-types*)
                                'latex
                                *org-mode-text-object-types*))
                 row))
              my-list))
       (list :text (org-table-to-latex my-list)
             :recurse nil
             :reparse nil
             :escape nil)))
    ((string= backend 'html)
     (org-table-to-html (text-object-property obj :table)))))

(defclass org-document (document)
  ()
  (:documentation "org-mode document."))

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
             ;; dont escape the commands in the preamble
             :escape t
             :escape-region inner-region)))
    ('html
     (concatenate 'string "org-html-doc" (string #\newline) (text-object-text obj)) t)))

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

;; A
(defun extract-babel-results-blocks (text)
  "return a list of lists (begin-idx end-idx substring) for all org-mode results blocks in TEXT.
- begin-idx is the index of the first occurrence of \"#+RESULTS\" in TEXT.
- end-idx is the index after the last contiguous line that starts with a colon (:) following the marker line.
- substring is the corresponding text.
the marker line may be of the form \"#+RESULTS:\" or \"#+RESULTS[...]:\".
returns NIL if no marker is found."
  (let (results
        (current-index 0)
        marker-index)
    (loop
      (setf marker-index (search "#+RESULTS" text :start2 current-index :test #'char-equal))
      (unless marker-index
        (return results))
      (let* ((len (length text))
             (subtext (subseq text marker-index))
             (nl-pos (search (string #\newline) subtext :test #'char-equal))
             (marker-line-end (if nl-pos (+ marker-index nl-pos) len))
             (begin marker-index)
             (block-end marker-line-end)
             (current (if (< marker-line-end len)
                          (1+ marker-line-end)
                          marker-line-end)))
        ;; loop over contiguous lines that start with a colon.
        (loop while (< current len)
              for current-subtext = (subseq text current)
              for nl-pos = (search (string #\newline) current-subtext :test #'char-equal)
              for abs-nl = (if nl-pos (+ current nl-pos) len)
              for line = (subseq text current abs-nl)
              while (and (> (length (string-trim " " line)) 0)
                         (char= (char (string-trim " " line) 0) #\:))
              do (progn
                   (setf block-end abs-nl)
                   (setf current (if (< abs-nl len)
                                     (1+ abs-nl)
                                     abs-nl))))
        (push (list begin block-end (subseq text begin block-end)) results)
        (setf current-index block-end)))
    (nreverse results)))

(defun parse-org-file (filepath)
  ;; we need to "finalize" the classes to be able to use MOP
  (dolist (mytype *org-mode-text-object-types*)
    (sb-mop:finalize-inheritance mytype))
  (let* ((result (parse (uiop:read-file-string filepath)
                        *org-mode-text-object-types*
                        :as-doc t
                        :relative-positions t
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