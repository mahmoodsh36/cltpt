(defpackage :cltpt/org-mode
  (:use :cl)
  (:export :org-list-matcher :org-header :org-list
   :*org-mode* :org-mode-text-object-types
   :org-block :init :*org-enable-macros*))

(in-package :cltpt/org-mode)

(defvar *org-enable-macros* nil)

(defun init ()
  (setf (cltpt/base:text-format-text-object-types *org-mode*)
        (concatenate
         'list
         ;; crucial to keep the order of types, especially types like
         ;; org-src-block and org-block because we want org-src-block
         ;; to be attempted first, otherwise org-block might match first
         '(org-list
           org-table
           org-header
           org-link
           org-timestamp
           org-src-block
           org-export-block
           org-block
           org-prop-drawer
           org-drawer
           org-latex-env
           org-keyword ;; has to be after org-latex-env and before cltpt/latex:latex-env
           cltpt/latex:display-math cltpt/latex:inline-math cltpt/latex:latex-env
           org-italic
           org-emph
           org-inline-code
           org-comment
           web-link)
         (when *org-enable-macros*
           ;; TODO: make it so that *org-enable-macros* is a non-org-specific var
           ;; and use it for all formats. handle it in text-format.lisp
           '(cltpt/base:text-macro
             cltpt/base:post-lexer-text-macro))))
  ;; (setf (cltpt/base:text-format-name *org-mode*)
  ;;       "org-mode")
  (setf (cltpt/base:text-format-document-type *org-mode*)
        'org-document))

(defvar *org-mode*
  (cltpt/base:make-text-format "org-mode")
  "`text-format' instance of the org-mode format.")

(defun org-mode-text-object-types ()
  (cltpt/base:text-format-text-object-types *org-mode*))

(defun org-mode-inline-text-object-types ()
  (intersection
   '(org-link web-link org-inline-code org-emph org-italic
     cltpt/latex:display-math cltpt/latex:inline-math cltpt/latex:latex-env)
   (org-mode-text-object-types)))

(defun copy-rule (rule id &key type)
  (if (cltpt/base:plistp rule)
      (let ((copy (copy-tree rule)))
        (setf (getf copy :id) id)
        (when type
          (setf (getf copy :type) type))
        copy)
      (if type
          (list :pattern (copy-tree rule) :id id :type type)
          (list :pattern (copy-tree rule) :id id))))

(defun copy-modify-rule (rule modifications)
  "copy a RULE, apply MODIFICATIONS to it.

MODIFICATIONS is an alist of the form (id . new-rule) where id is the subrule
to replace and new-rule is the rule to replace it with."
  (let ((new-rule (copy-tree rule)))
    (cltpt/tree:tree-map
     new-rule
     (lambda (subrule)
       (when (cltpt/base:plistp subrule)
         (loop for modification in modifications
               for modification-id = (car modification)
               for modification-rule = (cdr modification)
               do (if (equal (getf subrule :id) modification-id)
                      (setf (getf subrule :pattern) modification-rule))))))
    new-rule))

(defun org-mode-inline-text-object-rule ()
  (mapcar
   (lambda (subclass-name)
     (copy-rule
      (cltpt/base:text-object-rule-from-subclass subclass-name)
      subclass-name))
   (org-mode-inline-text-object-types)))

(defun org-mode-text-object-types-except (exceptions)
  (reverse
   (set-difference (org-mode-text-object-types)
                   exceptions
                   :test 'equal)))

(defvar *org-inline-text-objects-rule*
  '(eval
    (org-mode-inline-text-object-rule)))

;; even though a property drawer can be considered just a drawer with a special treatment, we implement it as a distinct object from `org-drawer'
(defvar *org-prop-drawer-rule*
  `(:pattern
    (cltpt/combinator:pair
     (cltpt/combinator:followed-by
      (:pattern (cltpt/combinator:literal-casein ":properties:")
       :id drawer-open-tag)
      cltpt/combinator:at-line-end-p)
     (cltpt/combinator:when-match
      (cltpt/combinator:literal-casein ":end:")
      cltpt/combinator:at-line-start-p)
     ((:pattern
       (cltpt/combinator:consec
        (cltpt/combinator:literal ":")
        (:pattern (cltpt/combinator:symbol-matcher)
         :id drawer-key)
        (cltpt/combinator:literal ":")
        (cltpt/combinator:atleast-one-discard (cltpt/combinator:literal " "))
        (:pattern (cltpt/combinator:all-but-newline)
         :id drawer-value))
       :id drawer-entry)))
    :on-char #\:))
(defclass org-prop-drawer (cltpt/base:text-object)
  ((cltpt/base::rule
    :allocation :class
    :initform *org-prop-drawer-rule*))
  (:documentation "org-mode properties drawer."))

;; ideally we should be using a hashmap, but it probably doesnt matter here.
(defmethod org-prop-drawer-get ((obj org-prop-drawer) key)
  (cdr (assoc key (cltpt/base:text-object-property obj :alist)
              :test 'equal)))

(defmethod (setf org-prop-drawer-get) (value (obj org-prop-drawer) key)
  (push (cons key value) (cltpt/base:text-object-property obj :alist)))

(defmethod org-prop-drawer-set ((obj org-prop-drawer) key value)
  (setf (org-prop-drawer-get obj key) value))

(defmethod org-prop-drawer-alist ((obj org-prop-drawer))
  (cltpt/base:text-object-property obj :alist))

(defmethod org-prop-drawer-keys ((obj org-prop-drawer))
  (mapcar 'car (org-prop-drawer-alist obj)))

(defmethod cltpt/base:text-object-init :after ((obj org-prop-drawer) str1 match)
  (loop for submatch in (cltpt/combinator:find-submatch-all match 'drawer-entry)
        do (let ((key (cltpt/combinator:find-submatch submatch 'drawer-key))
                 (val (cltpt/combinator:find-submatch submatch 'drawer-value)))
             (org-prop-drawer-set obj
                             (cltpt/combinator:match-text (car key))
                             (cltpt/combinator:match-text (car val))))))

;; simply dont convert drawers (this isnt the correct org-mode behavior tho)
;; TODO: properly convert drawers. drawers can include any elements but headers.
(defmethod cltpt/base:text-object-convert ((obj org-prop-drawer)
                                           (backend cltpt/base:text-format))
  "")

(defvar *org-keyword-rule*
  `(:pattern
    (cltpt/combinator:consec-atleast-one
     (cltpt/combinator:consec
      (cltpt/combinator:literal "#+")
      (:pattern (cltpt/combinator:symbol-matcher)
       :id keyword)
      (cltpt/combinator:literal ":"))
     (cltpt/combinator:atleast-one-discard (cltpt/combinator:literal " "))
     (cltpt/combinator:any
      ;; TODO: we shouldnt be enabling text-macros by default, even for keywords.
      ,(copy-rule
        cltpt/base::*post-lexer-text-macro-rule*
        'cltpt/base::post-lexer-text-macro)
      ;; capture an arbitrary sequence until an EOL (or EOF ofc)
      (:pattern (cltpt/combinator:all-but-newline)
       :id value)))
    :on-char #\#))
(defclass org-keyword (cltpt/base:text-object)
  ((cltpt/base::rule
    :allocation :class
    :initform *org-keyword-rule*))
  (:documentation "org-mode file-level keyword."))

(defun rule-with-org-keywords (rule &optional must-have-keywords)
  "take a parser rule, \"wrap\" it with other rules to match org-like keywords for the object.

by org-like keywords we mean things like '#+name: name-here'.
MUST-HAVE-KEYWORDS determines whether keywords must exist for a match to succeed."
  (if must-have-keywords
      `(cltpt/combinator:consec
        (cltpt/combinator:separated-atleast-one
         ,(string #\newline)
         ,(copy-rule *org-keyword-rule* 'org-keyword))
        ,(string #\newline)
        ,rule)
      `(cltpt/combinator:any
        (cltpt/combinator:consec
         (cltpt/combinator:separated-atleast-one
          ,(string #\newline)
          ,(copy-rule *org-keyword-rule* 'org-keyword))
         ,(string #\newline)
         ,rule)
        ,rule)))

(defmethod cltpt/base:text-object-finalize ((obj org-keyword))
  (let* ((match (cltpt/base:text-object-property obj :combinator-match))
         (value-match (car (cltpt/combinator:find-submatch match 'value)))
         (keyword-match (car (cltpt/combinator:find-submatch match 'keyword)))
         (value (cltpt/combinator:match-text value-match))
         (child (car (cltpt/base:text-object-children obj))))
    (unless value
      (when (typep child 'cltpt/base::post-lexer-text-macro)
        ;; if we get here, then the value is meant to be the evaluation result
        ;; of the post-lexer text macro.
        (setf value (cltpt/base::eval-post-lexer-macro child))))
    (setf (cltpt/base:text-object-property obj :value)
          value)
    (setf (cltpt/base:text-object-property obj :keyword)
          (cltpt/combinator:match-text keyword-match))))

(defmethod cltpt/base:text-object-convert ((obj org-keyword)
                                           (backend cltpt/base:text-format))
  (let* ((kw (cltpt/base:text-object-property obj :keyword))
         (value (cltpt/base:text-object-property obj :value))
         (final-result))
    ;; handle transclusions
    ;; (when (and kw (string= kw "transclude") cltpt/roam:*roam-convert-data*)
    ;;   (let* ((org-link-parse-result (cltpt/base:parse *org-mode* value))
    ;;          (first-child (car (cltpt/base:text-object-children
    ;;                             org-link-parse-result))))
    ;;     (when (typep first-child 'org-link)
    ;;       (let* ((dest (cltpt/base:text-object-property first-child :dest))
    ;;              (type (cltpt/base:text-object-property first-child :type))
    ;;              (rmr (getf cltpt/roam:*roam-convert-data* :roamer))
    ;;              (roam-link (cltpt/roam:resolve-link
    ;;                          rmr
    ;;                          (getf cltpt/roam:*roam-convert-data* :node)
    ;;                          obj
    ;;                          ;; if link doesnt have a type, treat it as an 'id' link
    ;;                          (if type
    ;;                              (intern type :cltpt/roam)
    ;;                              'cltpt/roam::file)
    ;;                          dest)))
    ;;         ;; just convert the linked node's object and return that
    ;;         (when roam-link
    ;;           (let* ((dest-node (cltpt/roam:link-dest-node roam-link))
    ;;                  (dest-text-obj (cltpt/roam:node-text-obj dest-node)))
    ;;             (let ((result (cltpt/base:convert-tree
    ;;                            dest-text-obj
    ;;                            *org-mode*
    ;;                            backend)))
    ;;               ;; TODO: for some reason redundant newlines in transcluded
    ;;               ;; documents dont get trimmed.
    ;;               (setf final-result
    ;;                     (list :text result
    ;;                           :reparse nil
    ;;                           :recurse nil
    ;;                           :escape nil)))))))))
    (or final-result
        (list :text ""
              :reparse t))))

(defvar *org-comment-rule*
  '(:pattern
    (cltpt/combinator:when-match
     (cltpt/combinator:consec
      (cltpt/combinator:literal "# ")
      (cltpt/combinator:all-but-newline))
     cltpt/combinator:at-line-start-p)
    :on-char #\#))
(defclass org-comment (cltpt/base:text-object)
  ((cltpt/base::rule
    :allocation :class
    :initform *org-comment-rule*))
  (:documentation "comment line in org-mode."))

(defmethod cltpt/base:text-object-convert ((obj org-comment)
                                           (backend cltpt/base:text-format))
  "")

(defvar *org-single-timestamp-rule*
  '(cltpt/combinator:consec-with-optional
    "<"
    (:pattern (cltpt/combinator:natural-number-matcher)
     :id year)
    "-"
    (:pattern (cltpt/combinator:natural-number-matcher)
     :id month)
    "-"
    (:pattern (cltpt/combinator:natural-number-matcher)
     :id day)
    (:pattern
     (cltpt/combinator:consec
      " "
      (:pattern (cltpt/combinator:word-matcher)
       :id weekday))
     :optional t)
    (:pattern
     (cltpt/combinator:consec
      " "
      (:pattern (cltpt/combinator:natural-number-matcher)
       :id hour)
      (cltpt/combinator:consec-atleast-one
       ":"
       (:pattern (cltpt/combinator:natural-number-matcher)
        :id minute)
       ":"
       (:pattern (cltpt/combinator:natural-number-matcher)
        :id second)))
     :optional t)
    (:pattern
     (cltpt/combinator:consec-atleast-one
      " +"
      (:pattern (cltpt/combinator:natural-number-matcher)
       :id repeat-num)
      (:pattern (cltpt/combinator:word-matcher)
       :id repeat-word))
     :optional t
     :id repeat)
    ">"))
(defvar *org-timestamp-rule*
  `(:pattern
    (cltpt/combinator:consec-atleast-one
     (:pattern ,*org-single-timestamp-rule*
      :id begin)
     "--"
     (:pattern ,*org-single-timestamp-rule*
      :id end))
    :on-char #\<))
(defclass org-timestamp (cltpt/base:text-object)
  ((cltpt/base::rule
    :allocation :class
    :initform *org-timestamp-rule*))
  (:documentation "a timestamp/date. e.g. <2023-12-28 Thu 18:30:00>."))

(defun org-timestamp-match-to-time (match)
  (let* ((day (parse-integer
               (cltpt/combinator:match-text
                (car (cltpt/combinator:find-submatch match 'day)))
               :junk-allowed t))
         (second-str (cltpt/combinator:match-text
                      (car (cltpt/combinator:find-submatch match 'second))))
         (second (when second-str (parse-integer second-str :junk-allowed t)))
         (year (parse-integer
                (cltpt/combinator:match-text
                 (car (cltpt/combinator:find-submatch match 'year)))
                :junk-allowed t))
         (month (parse-integer
                 (cltpt/combinator:match-text
                  (car (cltpt/combinator:find-submatch match 'month)))
                 :junk-allowed t))
         (hour-str (cltpt/combinator:match-text
                    (car (cltpt/combinator:find-submatch match 'hour))))
         (hour (when hour-str
                 (parse-integer hour-str :junk-allowed t)))
         (minute-str (cltpt/combinator:match-text
                      (car (cltpt/combinator:find-submatch match 'minute))))
         (minute (when minute-str
                   (parse-integer minute-str :junk-allowed t)))
         (weekday (car (cltpt/combinator:find-submatch match 'weekday))))
    (local-time:encode-timestamp 0
                                 (or second 0)
                                 (or minute 0)
                                 (or hour 0)
                                 day
                                 month
                                 year)))

(defvar *org-timestamp-bracket-rule*
  '(:pattern
    (cltpt/combinator:consec
     "["
     (:pattern (cltpt/combinator:natural-number-matcher)
      :id year)
     "-"
     (:pattern (cltpt/combinator:natural-number-matcher)
      :id month)
     "-"
     (:pattern (cltpt/combinator:natural-number-matcher)
      :id day)
     " "
     (:pattern (cltpt/combinator:word-matcher)
      :id weekday)
     (cltpt/combinator:consec-atleast-one
      " "
      (:pattern (cltpt/combinator:natural-number-matcher)
       :id hour)
      ":"
      (:pattern (cltpt/combinator:natural-number-matcher)
       :id minute)
      ":"
      (:pattern (cltpt/combinator:natural-number-matcher)
       :id second))
     "]")
    :on-char #\[))
(defclass org-timestamp-bracket (cltpt/base:text-object)
  ((cltpt/base::rule
    :allocation :class
    :initform *org-timestamp-bracket-rule*))
  (:documentation "a timestamp/date. e.g. [2023-12-28 Thu 18:30]."))

;; (defclass org-todo-state-timestamp (cltpt/base:text-object)
;;   ((cltpt/base::rule
;;     :allocation :class
;;     :initform ))
;;   (:documentation "e.g. the timestamp in CLOSED: [2023-12-28 Thu 19:32:11]"))

(defvar *org-list-rule*
  `(org-list-matcher
     ,*org-inline-text-objects-rule*))
(defclass org-list (cltpt/base:text-object)
  ((cltpt/base::rule
    :allocation :class
    :initform *org-list-rule*))
  (:documentation "org-mode list."))

;; this is very hacky, perhaps we should find a better way to export lists
;; and their children
(defmethod cltpt/base:text-object-convert ((obj org-list)
                                           (backend cltpt/base:text-format))
  ;; create a new non-org-list object, export it, use the output as a new list
  ;; reparse, that new list, then export the modified newly parsed list as if
  ;; it was the original
  (let* ((list-text (cltpt/base:text-object-text obj))
         (new-children (cltpt/base:text-object-children
                        (cltpt/base:parse
                         *org-mode*
                         list-text
                         :text-object-types (org-mode-inline-text-object-types))))
         (new-obj (make-instance 'cltpt/base:text-object)))
    (cltpt/base:text-object-init
     new-obj
     list-text
     (cltpt/base:text-object-property obj :combinator-match))
    (cltpt/base::text-object-force-set-text new-obj list-text)
    ;; set children of new-obj to those of obj without any nested org-lists
    ;; otherwise things wont work properly (because nested org-lists get converted)
    ;; to latex and later we try to parse them as a list
    (setf (cltpt/base:text-object-children new-obj) new-children)
    ;; we create a new intermediate object, treat it as raw text,
    ;; parse other types of text objects and convert them, then parse the result
    ;; as a list
    (let* ((new-txt (cltpt/base:convert-tree
                     new-obj
                     *org-mode*
                     backend
                     :reparse nil
                     :recurse t
                     :escape nil))
           (parsed (org-list-matcher nil new-txt 0)))
      (cond
        ((eq backend cltpt/latex:*latex*)
         (list :text (to-latex-list parsed)
               :recurse nil
               :reparse nil
               :escape nil))
        ((eq backend cltpt/html:*html*)
         (list :text (to-html-list parsed)
               :recurse nil
               :reparse nil
               :escape nil))))))

;; matching an org-list after a header we should only match if the list items
;; start with "State". otherwise its not a list that should be part of the
;; header's metadata.
(defun is-header-metadata-list (ctx rule str pos list-match)
  (uiop:string-prefix-p
   "State"
   (cltpt/combinator:match-text
    (car (cltpt/combinator:find-submatch
          list-match
          'list-item-content)))))

;; TODO: we may want to match tags-rule only if its before a newline
;; (let ((tags-rule
;;         '(cltpt/combinator:consec
;;           (cltpt/combinator:atleast-one-discard (cltpt/combinator:literal " "))
;;           (cltpt/combinator:literal ":")
;;           (cltpt/combinator:separated-atleast-one
;;            ":"
;;            (:pattern
;;             (cltpt/combinator:symbol-matcher)
;;             :id tag))
;;           (cltpt/combinator:literal ":")))
;;       (todo-rule
;;         '(:pattern (cltpt/combinator:upcase-word-matcher)
;;           :id todo-keyword)))
;;   (defvar *org-header-rule*
;;     `(:pattern
;;       (cltpt/combinator:consec-atleast-one
;;        (cltpt/combinator:when-match
;;         (cltpt/combinator:consec-with-optional
;;          (:pattern
;;           (cltpt/combinator:atleast-one-discard (cltpt/combinator:literal "*"))
;;           :id stars)
;;          (cltpt/combinator:atleast-one-discard (cltpt/combinator:literal " "))
;;          (:optional ,todo-rule)
;;          (cltpt/combinator:atleast-one-discard (cltpt/combinator:literal " "))
;;          (:pattern
;;           (cltpt/combinator:all-upto-without
;;            ,tags-rule
;;            ,(string #\newline))
;;           :id title)
;;          (:optional ,tags-rule))
;;         cltpt/combinator:at-line-start-p)
;;        ;; the following is for detecting metadata following an org header
;;        (cltpt/combinator:atleast-one
;;         (cltpt/combinator:consec
;;          (cltpt/combinator:literal ,(string #\newline))
;;          (cltpt/combinator:any
;;           (cltpt/combinator:separated-atleast-one
;;            " "
;;            (cltpt/combinator:any
;;             (:pattern
;;              (cltpt/combinator:consec
;;               (:pattern (cltpt/combinator:upcase-word-matcher)
;;                :id name)
;;               ": "
;;               ,(copy-rule *org-timestamp-rule* 'timestamp))
;;              :id action-active)
;;             (:pattern
;;              (cltpt/combinator:consec
;;               (:pattern (cltpt/combinator:upcase-word-matcher)
;;                :id name)
;;               ": "
;;               ,(copy-rule *org-timestamp-bracket-rule* 'timestamp))
;;              :id action-inactive)))
;;           ,(copy-rule *org-timestamp-rule* 'todo-timestamp)
;;           ,(copy-rule *org-prop-drawer-rule* 'org-prop-drawer)))))
;;       :on-char #\*)))
(let ((tags-rule
        '(cltpt/combinator:consec
          (cltpt/combinator:atleast-one-discard (cltpt/combinator:literal " "))
          (cltpt/combinator:literal ":")
          (cltpt/combinator:separated-atleast-one
           ":"
           (:pattern
            (cltpt/combinator:symbol-matcher)
            :id tag))
          (cltpt/combinator:literal ":")))
      (todo-rule
        '(:pattern (cltpt/combinator:upcase-word-matcher)
          :id todo-keyword)))
  (defvar *org-header-rule*
    `(:pattern
      (cltpt/combinator:consec-atleast-one
       (cltpt/combinator:when-match
        (cltpt/combinator:any
         ;; capture header with TODO keyword and tags
         (cltpt/combinator:consec
          (:pattern
           (cltpt/combinator:atleast-one-discard (cltpt/combinator:literal "*"))
           :id stars)
          (cltpt/combinator:atleast-one-discard (cltpt/combinator:literal " "))
          ,todo-rule
          (cltpt/combinator:atleast-one-discard (cltpt/combinator:literal " "))
          (:pattern
           (cltpt/combinator:all-upto-without
            ,tags-rule
            ,(string #\newline))
           :id title)
          ,tags-rule)
         ;; capture header without todo keyword but with tags
         (cltpt/combinator:consec
          (:pattern
           (cltpt/combinator:atleast-one-discard (cltpt/combinator:literal "*"))
           :id stars)
          (cltpt/combinator:atleast-one-discard (cltpt/combinator:literal " "))
          (:pattern
           (cltpt/combinator:all-upto-without
            ,tags-rule
            ,(string #\newline))
           :id title)
          ,tags-rule)
         ;; capture header with todo keyword but without tags
         (cltpt/combinator:consec
          (:pattern
           (cltpt/combinator:atleast-one-discard (cltpt/combinator:literal "*"))
           :id stars)
          (cltpt/combinator:atleast-one-discard (cltpt/combinator:literal " "))
          ,todo-rule
          (cltpt/combinator:atleast-one-discard (cltpt/combinator:literal " "))
          (:pattern (cltpt/combinator:all-but-newline)
           :id title))
         ;; capture header without todo keyword or tags
         (cltpt/combinator:consec
          (:pattern
           (cltpt/combinator:atleast-one-discard (cltpt/combinator:literal "*"))
           :id stars)
          (cltpt/combinator:atleast-one-discard (cltpt/combinator:literal " "))
          (:pattern (cltpt/combinator:all-but-newline)
           :id title)))
        cltpt/combinator:at-line-start-p)
       ;; the following is for detecting metadata following an org header
       (cltpt/combinator:atleast-one
        (cltpt/combinator:consec
         (cltpt/combinator:literal ,(string #\newline))
         (cltpt/combinator:any
          (cltpt/combinator:separated-atleast-one
           " "
           (cltpt/combinator:any
            (:pattern
             (cltpt/combinator:consec
              (:pattern (cltpt/combinator:upcase-word-matcher)
               :id name)
              ": "
              ,(copy-rule *org-timestamp-rule* 'timestamp
                          :type 'org-timestamp))
             :id action-active)
            (:pattern
             (cltpt/combinator:consec
              (:pattern (cltpt/combinator:upcase-word-matcher)
               :id name)
              ": "
              ,(copy-rule *org-timestamp-bracket-rule* 'timestamp))
             :id action-inactive)))
          ,(copy-rule *org-timestamp-rule* 'todo-timestamp
                      :type 'org-timestamp)
          (cltpt/combinator:when-match-after
           ,(copy-rule *org-list-rule* 'org-list)
           is-header-metadata-list)
          ,(copy-rule *org-prop-drawer-rule* 'org-prop-drawer)))))
      :on-char #\*)))
(defclass org-header (cltpt/base:text-object)
  ((cltpt/base::rule
    :allocation :class
    :initform *org-header-rule*))
  (:documentation "org-mode header."))

(defmethod cltpt/base:text-object-init :after ((obj org-header) str1 match)
  (let* ((stars (car (cltpt/combinator:find-submatch match 'stars)))
         (title (car (cltpt/combinator:find-submatch match 'title))))
    (setf (cltpt/base:text-object-property obj :level)
          (length (cltpt/combinator:match-text stars)))
    (setf (cltpt/base:text-object-property obj :title)
          (cltpt/combinator:match-text title))
    (setf (cltpt/base:text-object-property obj :tags)
          (loop for match in (cltpt/combinator:find-submatch-all match 'tag)
                collect (cltpt/combinator:match-text (car match))))
    ;; :initial-match-length will contain the length of the string that was matched
    ;; for the header's metadata (including title, tags, org-agenda metadata etc).
    ;; it is later used to detect where the contents of the headers actually start.
    (setf (cltpt/base:text-object-property obj :initial-match-length)
          (- (getf (car match) :end) (getf (car match) :begin)))))

(defun get-repeat-interval (repeat-num repeat-word)
  (let ((repeat-num (parse-integer repeat-num :junk-allowed t)))
    (cond
      ((equal repeat-word "w")
       (list :week repeat-num))
      ((equal repeat-word "d")
       (list :day repeat-num))
      ((equal repeat-word "h")
       (list :hour repeat-num)))))

(defun repeat-interval-from-timestamp-match (ts-match)
  (let ((repeat-num
          (cltpt/combinator:match-text
           (car (cltpt/combinator:find-submatch ts-match 'repeat-num))))
        (repeat-word
          (cltpt/combinator:match-text
           (car (cltpt/combinator:find-submatch ts-match 'repeat-word)))))
    (when (and repeat-num repeat-word)
      (get-repeat-interval repeat-num repeat-word))))

(defun handle-time-match (ts-match &optional (record (cltpt/agenda:make-task-record)))
  (let* ((begin-ts-match
           (cltpt/combinator:find-submatch ts-match 'begin))
         (end-ts-match
           (cltpt/combinator:find-submatch ts-match 'end))
         (begin-time (org-timestamp-match-to-time begin-ts-match))
         (end-time
           (when end-ts-match
             (org-timestamp-match-to-time end-ts-match)))
         (repeat-interval
           (repeat-interval-from-timestamp-match begin-ts-match)))
    (if end-time
        (setf (cltpt/agenda:task-record-time record)
              (cltpt/agenda:make-time-range
               :begin begin-time
               :end end-time))
        (setf (cltpt/agenda:task-record-time record)
              begin-time))
    (setf (cltpt/agenda:task-record-repeat record) repeat-interval)
    record))

(defmethod cltpt/base:text-object-finalize ((obj org-header))
  (let* ((match (cltpt/base:text-object-property obj :combinator-match))
         (title-match (car (cltpt/combinator:find-submatch match 'title)))
         (header-id)
         (task-records)
         (todo-keyword-match
           (car (cltpt/combinator:find-submatch match 'todo-keyword)))
         (timestamp-matches
           (cltpt/combinator:find-submatch-all match 'todo-timestamp))
         (action-active-matches
           (cltpt/combinator:find-submatch-all match 'action-active))
         (action-inactive-matches
           (cltpt/combinator:find-submatch-all match 'action-inactive)))
    (loop for ts-match in timestamp-matches
          do (let ((new-record (handle-time-match ts-match)))
               (push new-record task-records)))
    (labels ((is-drawer (obj2)
               (typep obj2 'org-prop-drawer)))
      (let ((drawers (cltpt/base:find-children obj #'is-drawer)))
        (loop for drawer in drawers
              do (loop for (key . value) in (org-prop-drawer-alist drawer)
                       do (cond
                            ((string-equal key "id")
                             (setf header-id value))
                            ((string-equal key "last_repeat")
                             ;; TODO
                             )
                            )))))
    (loop for action-match in action-active-matches
          do (let ((action-name
                     (cltpt/combinator:match-text
                      (car (cltpt/combinator:find-submatch action-match 'name))))
                   (action-timestamp
                     (cltpt/combinator:find-submatch action-match 'timestamp)))
               (cond
                 ((string-equal action-name "scheduled")
                  (push (handle-time-match
                         action-timestamp
                         (cltpt/agenda:make-record-scheduled))
                        task-records))
                 ((string-equal action-name "deadline")
                  (push (handle-time-match
                         action-timestamp
                         (cltpt/agenda:make-record-deadline))
                        task-records))
                 ((string-equal action-name "closed")
                  ))))
    (setf (cltpt/base:text-object-property obj :id) header-id)
    ;; initialize roam data
    (setf (cltpt/base:text-object-property obj :roam-node)
          (cltpt/roam:make-node
           :id header-id
           :title (cltpt/combinator:match-text title-match)
           :desc nil
           :text-obj obj))
    (when todo-keyword-match
      (let ((task (cltpt/agenda:make-task
                   :title (cltpt/combinator:match-text title-match)
                   :description nil
                   :state (cltpt/agenda:state-by-name
                           (cltpt/combinator:match-text todo-keyword-match))
                   :tags nil
                   :records task-records)))
        (loop for record in task-records
              do (setf (cltpt/agenda:task-record-task record) task))
        (setf (cltpt/base:text-object-property obj :task) task)))))

(defmethod cltpt/base:text-object-convert ((obj org-header)
                                           (backend cltpt/base:text-format))
  (let ((to-not-export
          (member "noexport"
                  (cltpt/base:text-object-property
                   obj
                   :tags)
                  :test 'equal)))
    (if to-not-export
        (list :text "" :reparse t)
        (let* ((obj-text (cltpt/base:text-object-text obj))
               (changes)
               (match (cltpt/base:text-object-property obj :combinator-match))
               (title-match (car (cltpt/combinator:find-submatch match 'title)))
               (match-begin (getf (car match) :begin))
               (match-end (getf (car match) :end))
               (close-tag
                 (cltpt/base:pcase backend
                   (cltpt/html:*html*
                    (format nil
                            "</h~A>"
                            ;; we want to start from h2, not h1 in html.
                            (1+ (cltpt/base:text-object-property obj :level))))
                   (cltpt/latex:*latex* "}")))
               (open-tag
                 (cltpt/base:pcase backend
                   (cltpt/html:*html*
                    (format nil
                            "<h~A>"
                            ;; we want to start from h2, not h1 in html.
                            (1+ (cltpt/base:text-object-property obj :level))))
                   (cltpt/latex:*latex*
                    (format nil
                            "\\~Asection{"
                            (cltpt/base:str-dupe
                             "sub"
                             (cltpt/base:text-object-property obj :level))))))
               (postfix-begin (- (getf title-match :end) match-begin))
               ;; use 1+ to account for the extra newline at the end which
               ;; we want removed
               (postfix-end (min (1+ (- match-end match-begin))
                                 (length obj-text)))
               (prefix-end (- (getf title-match :begin) match-begin))
               ;; the "old postfix" region is the region containing the
               ;; tags, and the metadata after the tags+newline
               (old-postfix-region
                 (cltpt/base:make-region
                  :begin postfix-begin
                  :end postfix-end))
               ;; the "old prefix" region is the region containing the TODO
               ;; keyword
               (old-prefix-region
                 (cltpt/base:make-region
                  :begin 0
                  :end prefix-end)))
          ;; remove the children in the metadata region
          (loop for child in (copy-seq (cltpt/base:text-object-children obj))
                do (when (cltpt/base:region-contains
                          old-postfix-region
                          (cltpt/base:text-object-begin child))
                     (setf (cltpt/base:text-object-children obj)
                           (delete child
                                   (cltpt/base:text-object-children obj)))))
          ;; change the "postfix" text after the title (tags etc)
          (push (cons close-tag old-postfix-region) changes)
          ;; change the "prefix" text before the title (stars etc)
          (push (cons open-tag old-prefix-region) changes)
          (list
           :text obj-text
           :remove-newline-after t
           :changes changes
           :escape t
           ;; escape regions are the title, and the contents of the header
           :escape-regions
           (list
            (cltpt/base:make-region
             :begin (cltpt/base:region-end old-prefix-region)
             :end (cltpt/base:region-begin old-postfix-region))
            (cltpt/base:make-region
             :begin (cltpt/base:region-end old-postfix-region)
             :end (length obj-text)))
           :reparse nil
           :recurse t)))))

(defvar *org-link-rule*
  '(:pattern
    (cltpt/combinator:any
     ;; [[type:dest][desc]]
     (cltpt/combinator:consec
      "[["
      (:pattern (cltpt/combinator:symbol-matcher) :id link-type)
      ":"
      (:pattern (cltpt/combinator:all-but "[]") :id link-dest)
      "]["
      (:pattern (cltpt/combinator:all-but "[]") :id link-desc)
      "]]")
     ;; [[type:dest]]
     (cltpt/combinator:consec
      "[["
      (:pattern (cltpt/combinator:symbol-matcher) :id link-type)
      ":"
      (:pattern (cltpt/combinator:all-but "[]") :id link-dest)
      "]]")
     ;; [[dest]]
     (cltpt/combinator:consec
      "[["
      (:pattern (cltpt/combinator:all-but "[]") :id link-dest)
      "]]"))
    :on-char #\[))
(defclass org-link (cltpt/base:text-link)
  ((cltpt/base::rule
    :allocation :class
    :initform *org-link-rule*))
  (:documentation "org-mode link."))

;; we're not being clever about it
(defvar *web-link-rule*
  `(:pattern
    (cltpt/combinator:consec
     "https://"
     (cltpt/combinator:all-but-whitespace))
    :on-char #\h))
(defclass web-link (cltpt/base:text-object)
  ((cltpt/base::rule
    :allocation :class
    :initform *web-link-rule*))
  (:documentation "a web link."))

(defmethod cltpt/base:text-object-init :after ((obj web-link) str1 match)
  (setf (cltpt/base:text-object-property obj :is-inline)
        t))

(defmethod cltpt/base:text-object-convert ((obj web-link) (backend cltpt/base:text-format))
  (cond
    ((eq backend cltpt/html:*html*)
     (list :text
           (format nil
                   "<a href='~A'>~A</a>"
                   (cltpt/base:text-object-text obj)
                   (cltpt/base:text-object-text obj))
           :escape nil))))

(defvar *org-inline-code-rule*
  '(:pattern
    (cltpt/combinator:pair
     (cltpt/combinator:unescaped (cltpt/combinator:literal "~"))
     (cltpt/combinator:unescaped (cltpt/combinator:literal "~"))
     nil nil nil)
    :escapable #\~
    :on-char #\~))
(defclass org-inline-code (cltpt/base:text-object)
  ((cltpt/base::rule
    :allocation :class
    :initform *org-inline-code-rule*))
  (:documentation "org-mode inline code (surrounded by tildes)."))

(defmethod cltpt/base:text-object-init :after ((obj org-inline-code) str1 match)
  (setf (cltpt/base:text-object-property obj :is-inline)
        t))

(defmethod cltpt/base:text-object-finalize ((obj org-inline-code))
  (compress-contents-region-by-one obj))

(defmethod cltpt/base:text-object-convert ((obj org-inline-code)
                                           (backend cltpt/base:text-format))
  (cond
    ((eq backend cltpt/latex:*latex*)
     (cltpt/base:rewrap-within-tags
      obj
      "\\verb{"
      "}"))
    ((eq backend cltpt/html:*html*)
     (cltpt/base:rewrap-within-tags
      obj
      "<code>"
      "</code>"))))

(defclass org-table (cltpt/base:text-object)
  ((cltpt/base::rule
    :allocation :class
    :initform `(:pattern
                (org-table-matcher
                 ;; TODO: currently enabling some text objects like org-italic
                 ;; inside tables causes issues. this happens when there
                 ;; is something like `| cell / 1 | cell / 2 |` where the
                 ;; forward slashes may correspond to an org-italic instance
                 ;; which might break the text-object tree.
                 ;; TODO: its not good that we're including all rules as
                 ;; symbols here. it would cause alot of hashmap lookups
                 ;; during parsing.
                 (eval
                  (set-difference
                   (org-mode-inline-text-object-types)
                   '(org-italic org-emph))))
                :on-char #\|)))
  (:documentation "org-mode table."))

;; TODO: this uses nconc all the time which causes squared complexity
(defmethod cltpt/base:text-object-convert ((obj org-table)
                                           (backend cltpt/base:text-format))
  (let* ((match (cltpt/base:text-object-property obj :combinator-match))
         (changes)
         (match-begin (getf (car match) :begin))
         (escape-regions)
         (open-tag
           (cltpt/base:pcase backend
             (cltpt/html:*html* "<table>")
             (cltpt/latex:*latex*
              (let* ((first-row-match (cltpt/combinator:find-submatch
                                       match
                                       'table-row))
                     (num-cols
                       (length
                        (cltpt/combinator:find-submatch-all
                         match
                         'table-cell))))
                (format nil
                        "\\begin{tabular} { |~{~a~^|~}| } \\hline~%"
                        (loop repeat num-cols collect "l"))))))
         (close-tag
           (cltpt/base:pcase backend
             (cltpt/html:*html* "</table>")
             (cltpt/latex:*latex* "\\hline\\end{tabular}")))
         ;; if separators are defined, they should be used along with
         ;; open/close tags. so the behavior may differ from one format
         ;; to another.
         (row-open-tag (cltpt/base:pcase backend
                         (cltpt/html:*html* "<tr>")))
         (row-close-tag (cltpt/base:pcase backend
                          (cltpt/html:*html* "</tr>")))
         (cell-open-tag (cltpt/base:pcase backend
                          (cltpt/html:*html* "<td>")))
         (cell-close-tag (cltpt/base:pcase backend
                           (cltpt/html:*html* "</td>")))
         (cell-separator (cltpt/base:pcase backend
                           (cltpt/latex:*latex* "&")))
         (row-separator (cltpt/base:pcase backend
                          (cltpt/latex:*latex* "\\hline\\\\"))))
    ;; opening tag
    (setf changes
          (list (cons open-tag
                      (cltpt/base:make-region
                       :begin 0
                       :end 0))))
    ;; handle table cells
    (loop for row-match in (cdr match)
          for is-first-row = t then nil
          do (case (getf (car row-match) :id)
               ('table-row
                (let ((final-open-tag
                        (cltpt/base:concat
                         (remove nil
                                 (list (unless is-first-row row-separator)
                                       row-open-tag))
                         'string)))
                  (nconc changes
                         (list (cons final-open-tag
                                     (cltpt/base:region-decf
                                      (cltpt/base:make-region
                                       :begin (getf (car row-match) :begin)
                                       :end (getf (car row-match) :begin))
                                      match-begin)))))
                (loop for cell-match
                        in (cltpt/combinator:find-submatch-all
                            row-match
                            'table-cell)
                      for is-first-cell = t then nil
                      do (let ((final-open-tag
                                 (cltpt/base:concat
                                  (remove
                                   nil
                                   (list (unless is-first-cell cell-separator)
                                         cell-open-tag))
                                  'string)))
                           (nconc changes
                                  (list
                                   (cons
                                    final-open-tag
                                    (cltpt/base:region-decf
                                     (cltpt/base:make-region
                                      ;; we use 1- to get the space at the start
                                      ;; of each cell. this prevents the handle-changed-regions
                                      ;; function from thinking the change
                                      ;; is part of a child when it shouldnt be
                                      ;; we use another 1- to get a | replaced too.
                                      :begin (1- (1- (getf (car cell-match) :begin)))
                                      :end (getf (car cell-match) :begin))
                                     match-begin)))))
                         (when cell-close-tag
                           (nconc
                            changes
                            (list
                             (cons cell-close-tag
                                   (cltpt/base:region-decf
                                    (cltpt/base:make-region
                                     :begin (getf (car cell-match) :end)
                                     :end (getf (car cell-match) :end))
                                    match-begin))))))
                (nconc changes
                       (list (cons row-close-tag
                                   (cltpt/base:region-decf
                                    (cltpt/base:make-region
                                     ;; we add 1- to get the | at the end of the row
                                     :begin (1- (getf (car row-match) :end))
                                     :end (getf (car row-match) :end))
                                    match-begin)))))
               ('table-hrule
                (nconc changes
                       (list (cons ""
                                   (cltpt/base:region-decf
                                    (cltpt/base:make-region
                                     :begin (getf (car row-match) :begin)
                                     :end (getf (car row-match) :end))
                                    match-begin)))))))
    ;; ending tag
    (nconc changes
           (list (cons close-tag
                       (cltpt/base:make-region
                        :begin (length (cltpt/base:text-object-text obj))
                        :end (length (cltpt/base:text-object-text obj))))))
    (list :text (cltpt/base:text-object-text obj)
          :changes changes
          :escape nil
          :reparse nil
          :recurse t)))

(defclass org-document (cltpt/base:document)
  ()
  (:documentation "org-mode document."))

(defmethod handle-parsed-org-keywords ((obj cltpt/base:text-object))
  "takes a text object that was matched with instances of `org-keyword', collect the keywords and their values into an alist and return it."
  (let ((result-alist)
        ;; is-first is only for `org-document' which may contain a drawer
        ;; as the first element. even though this would also affect other elements
        ;; in which the org keywords are meant to be at the top, it is probably
        ;; harmless, but maybe we should fix it anyway.
        ;; TODO: only skip first for `org-document'.
        (is-first t))
    (loop for child in (cltpt/base:text-object-children obj)
          while (or (typep child 'org-keyword) is-first)
          when (typep child 'org-keyword)
            do (let ((kw-name (cltpt/base:text-object-property child :keyword))
                     (kw-value (cltpt/base:text-object-property child :value)))
                 (push (cons kw-name kw-value) result-alist)
                 (setf is-first nil)))
    result-alist))

;; often we need to grab the value from the org-keyword object itself. because
;; the value is dynamic, it might change after parsing. storing it might
;; not be enough for now.
;; TODO: optimize.
(defmethod text-object-org-keyword-value ((obj cltpt/base:text-object) kw-name)
  (let ((my-alist (handle-parsed-org-keywords obj)))
    (cltpt/base:alist-get my-alist kw-name)))

(defmethod cltpt/base:text-object-finalize ((obj org-document))
  (let* ((doc-title)
         (doc-id)
         (doc-date)
         (doc-tags)
         (first-child (car (cltpt/base:text-object-children obj)))
         (first-child-is-drawer (typep first-child 'org-prop-drawer)))
    ;; detect id and title of document, either through property drawer at the top
    ;; or using keywords
    (when first-child-is-drawer
      (setf doc-id (org-prop-drawer-get first-child "ID")))
    (setf (cltpt/base:text-object-property obj :keywords-alist)
          (handle-parsed-org-keywords obj))
    (setf doc-title
          (cltpt/base:alist-get (cltpt/base:text-object-property obj :keywords-alist)
                     "title"))
    ;; denote-style identifier
    (setf doc-id
          (cltpt/base:alist-get (cltpt/base:text-object-property obj :keywords-alist)
                     "identifier"))
    (setf doc-date
          (cltpt/base:alist-get (cltpt/base:text-object-property obj :keywords-alist)
                     "date"))
    (let ((tags-str (cltpt/base:alist-get (cltpt/base:text-object-property obj :keywords-alist)
                               "filetags")))
      ;; avoid first and last ':', split by ':' to get tags
      (setf doc-tags (cltpt/base:str-split (cltpt/base:subseq* tags-str 1 -1) ":")))
    ;; set metadata in the object itself
    (setf (cltpt/base:text-object-property obj :title) doc-title)
    (setf (cltpt/base:text-object-property obj :tags) doc-tags)
    (setf (cltpt/base:text-object-property obj :id) doc-id)
    (setf (cltpt/base:text-object-property obj :date) doc-date)
    ;; initialize roam data
    (setf (cltpt/base:text-object-property obj :roam-node)
          (cltpt/roam:make-node
           :id doc-id
           :title doc-title
           :desc nil
           :text-obj obj))
    (let ((header-stack))
      (loop for child in (cltpt/base:text-object-children obj)
            for last-header = (car header-stack)
            do (if (typep child 'org-header)
                   (let ((popped-header child)
                         (this-header-level (cltpt/base:text-object-property
                                             child
                                             :level))
                         (last-header-level
                           (when last-header
                             (cltpt/base:text-object-property last-header :level))))
                     ;; if any headers have a "higher" or equal level, we should
                     ;; discard them and reveal a parent fit to inherit this new
                     ;; header (if any). also, the new header should be at the front
                     ;; of the stack.
                     (loop while (and last-header-level
                                      (<= this-header-level last-header-level))
                           do ;; TODO: optimize this, we are running subseq repeatedly here.
                              (cltpt/base:text-object-extend-in-parent
                               last-header
                               (cltpt/base:text-object-begin child))
                              (setf popped-header (pop header-stack))
                              (setf last-header (car header-stack))
                              (setf last-header-level
                                    (when last-header
                                      (cltpt/base:text-object-property
                                       last-header
                                       :level))))
                     ;; the contents of the header needs to start where the metadata
                     ;; and the title text end.
                     (setf (cltpt/base:text-object-property child :contents-region)
                           (cltpt/base:make-region
                            :begin (cltpt/base:text-object-property
                                    child
                                    :initial-match-length)
                            :end nil))
                     (push child header-stack))
                   ;; if this object not a header, and we have a last header, just
                   ;; set this as a child of this last header.
                   (when last-header
                     ;; TODO: optimize this, this is very slow.
                     (cltpt/base:text-object-move child last-header))))
      ;; at the end, the text after the last header (if any) needs to be moved
      ;; into the region of the last header (again, if any. as in, if any headers
      ;; exist at all).
      ;; TODO: note that this behavior is similar to org-mode's in that there is no
      ;; way to prevent some text to be detached from a specific header if the header
      ;; precedes it in the original text. while this should be default behavior,
      ;; maybe we can make it so that its not the only behavior, maybe we should
      ;; support different and independent header hierarchies in the same file,
      ;; because i have personally seen many complaints about this not being possible
      ;; in org-mode itself. this is feasible but will require more work, but for
      ;; now this will be sufficient.
      (when header-stack
        ;; first extend the last header to the end of the document
        (let ((last-last-header (cltpt/base:last-atom header-stack)))
          (cltpt/base:text-object-extend-in-parent
           last-last-header
           (cltpt/base:text-object-end obj)))
        ;; then ensure all headers encompass their children
        (dolist (header (reverse header-stack)) ; process from deepest to shallowest
          (let ((children (cltpt/base:text-object-children header)))
            (when children
              (let* ((header-begin (cltpt/base:text-object-begin header))
                     (header-end (cltpt/base:text-object-end header))
                     (max-child-abs-end
                       (reduce #'max
                               children
                               :key (lambda (child)
                                      (+ (cltpt/base:text-object-begin header)
                                         (cltpt/base:text-object-end child))))))
                (when (> max-child-abs-end header-end)
                  (cltpt/base:text-object-extend-in-parent
                   header
                   max-child-abs-end))))))))))

(defun ensure-latex-previews-generated (org-doc)
  (let ((mylist))
    (cltpt/base:map-text-object
     org-doc
     (lambda (obj)
       (when (or (typep obj 'cltpt/latex:inline-math)
                 (typep obj 'cltpt/latex:display-math)
                 (typep obj 'cltpt/latex:latex-env))
         (push (cltpt/base:text-object-contents obj) mylist))))
    (cltpt/latex:generate-previews-for-latex mylist)))

(defmethod cltpt/base:text-object-convert ((obj org-document)
                                           (backend cltpt/base:text-format))
  (list :text (cltpt/base:text-object-text obj)
        :escape t
        :reparse nil
        :recurse t))

(defclass org-emph (cltpt/base:text-object)
  ((cltpt/base::rule
    :allocation :class
    :initform `(:pattern
                (cltpt/combinator:flanked-by-whitespace
                 (cltpt/combinator:pair
                  (cltpt/combinator:unescaped (cltpt/combinator:literal "*"))
                  (cltpt/combinator:unescaped (cltpt/combinator:literal "*"))
                  nil
                  nil
                  nil))
                :escapable #\*
                :on-char #\*)))
  (:documentation "org-mode emphasized text (surrounded by asterisks)."))

(defmethod cltpt/base:text-object-init :after ((obj org-emph) str1 match)
  (setf (cltpt/base:text-object-property obj :is-inline)
        t))

;; this function shouldnt have a repeated effect on repeated calls, since
;; it is called in finalization methods and those are called when
;; handling incremental changes.
(defun compress-contents-region-by-one (obj)
  (setf (cltpt/base:text-object-property obj :contents-region)
        (cltpt/base:make-region
         :begin 0
         :end (cltpt/base:region-length
               (cltpt/base:text-object-text-region obj))))
  (cltpt/base:region-compress
   (cltpt/base:text-object-property obj :contents-region)
   1
   1))

(defmethod cltpt/base:text-object-finalize ((obj org-emph))
  (compress-contents-region-by-one obj))

(defmethod cltpt/base:text-object-convert ((obj org-emph) (backend cltpt/base:text-format))
  (cond
    ((eq backend cltpt/latex:*latex*)
     (cltpt/base:rewrap-within-tags obj "\\textbf{" "}"))
    ((eq backend cltpt/html:*html*)
     (cltpt/base:rewrap-within-tags obj "<b>" "</b>"))))

;; TODO: in org-mode, slashes are interpreted as italic text only if they are
;; preceded/succeeded by spaces.
(defvar *org-italic-rule*
  '(:pattern
    (cltpt/combinator:flanked-by-whitespace
     (cltpt/combinator:pair
      (cltpt/combinator:unescaped (cltpt/combinator:literal "/"))
      (cltpt/combinator:unescaped (cltpt/combinator:literal "/"))
      nil
      nil
      nil))
    :on-char #\/))
(defclass org-italic (cltpt/base:text-object)
  ((cltpt/base::rule
    :allocation :class
    :initform *org-italic-rule*))
  (:documentation "org-mode italicized text (surrounded by forward slahes)."))

(defmethod cltpt/base:text-object-init :after ((obj org-italic) str1 match)
  (setf (cltpt/base:text-object-property obj :is-inline)
        t))

(defmethod cltpt/base:text-object-finalize ((obj org-italic))
  (compress-contents-region-by-one obj))

(defmethod cltpt/base:text-object-convert ((obj org-italic)
                                           (backend cltpt/base:text-format))
  (cond
    ((eq backend cltpt/latex:*latex*)
     (cltpt/base:rewrap-within-tags obj "\\textit{" "}"))
    ((eq backend cltpt/html:*html*)
     (cltpt/base:rewrap-within-tags obj "<i>" "</i>"))))

;; wrap text for exporting within specific "tags".
(defun within-tags (open-tag inner-text close-tag
                    &key (reparse t) (escape t))
  (let* ((text (concatenate 'string
                            open-tag
                            inner-text
                            close-tag))
         (inner-region (cltpt/base:make-region
                        :begin (length open-tag)
                        :end (- (length text) (length close-tag)))))
    (list :text text
          :recurse t
          :reparse-region inner-region
          :escape-region inner-region
          :reparse reparse
          :escape escape)))

;; we dont want a keyword to be matched as a value, e.g. ":keyword1 :keyword2".
;; so we discard any match starting with ":".
(let ((not-starting-with-colon
        (lambda (ctx str pos)
          (not (char= (char str pos) #\:)))))
  (defvar *keywords-rule*
    `(:pattern
      (cltpt/combinator:separated-atleast-one
       (cltpt/combinator:literal " ")
       (cltpt/combinator:any
        ;; this should capture strings or lisp expressions provided
        ;; as a value of a keyword
        (:pattern
         (cltpt/combinator:consec
          (cltpt/combinator:literal ":")
          (:pattern (cltpt/combinator:symbol-matcher)
           :id keyword)
          (cltpt/combinator:literal " ")
          (cltpt/combinator:when-match
           (cltpt/combinator:succeeded-by
            (:pattern (cltpt/combinator:lisp-sexp)
             :id value)
            ;; we make sure to capture lisp expressions only if they are succeeded
            ;; by " :" because otherwise we might capture a word that is part
            ;; of a sequence of words, like ":title my title", which is a case
            ;; that should be handled by the following rule, not this one.
            (cltpt/combinator:literal " :"))
           ,not-starting-with-colon))
         :id keywords-entry)
        ;; a keywords with a value
        (:pattern
         (cltpt/combinator:consec-atleast-one
          ;; keyword
          (cltpt/combinator:consec
           (cltpt/combinator:literal ":")
           (:pattern (cltpt/combinator:symbol-matcher)
            :id keyword))
          ;; value
          (cltpt/combinator:consec
           (cltpt/combinator:literal " ")
           ;; capture all until the next " :", or until the line ends.
           (:pattern
            (cltpt/combinator:when-match
             (cltpt/combinator:all-upto
              (cltpt/combinator:any
               (cltpt/combinator:literal ,(string #\newline))
               (cltpt/combinator:literal " :")))
             ,not-starting-with-colon)
            :id value)))
         :id keywords-entry)))
      :id keywords)))

(defvar *org-babel-results-rule*
  `(:pattern
    (cltpt/combinator:consec
     (cltpt/combinator:any
      (cltpt/combinator:literal-casein "#+results:")
      (cltpt/combinator:consec
       (cltpt/combinator:literal-casein "#+results[")
       (cltpt/combinator:symbol-matcher)
       (cltpt/combinator:literal "]:")))
     (cltpt/combinator:literal ,(string #\newline))
     (:pattern
      (cltpt/combinator:any
       #| detect syntax like
       #+RESULTS[c419e84a898b0cdd18b49e14c750b71743921b86]:
       : (5 1 7 4)
       : more text
       : more text
       |#
       (cltpt/combinator:separated-atleast-one
        ,(string #\newline)
        (cltpt/combinator:consec
         (:pattern
          (cltpt/combinator:any
           (cltpt/combinator:consec
            (cltpt/combinator:literal ": ")
            (cltpt/combinator:all-but-newline))
           (cltpt/combinator:literal ": "))
          :id output-line)))
       #| detect syntax like
       #+RESULTS[ca08ab2a6a58662675694033105ab0b331611fa2]:
       [[file:/tmp/jyBtMrE.svg]]

       the possible elements could be org-link, org-table, org-block
       |#
       (cltpt/combinator:any
        org-export-block
        ,(copy-rule *org-list-rule* 'org-list)
        org-table
        org-block
        org-drawer
        ,(copy-rule *org-link-rule* 'org-link)))
      :id results-content))
    :id results))
(defvar *org-src-block-no-kw-rule*
  `(cltpt/combinator:pair
    (cltpt/combinator:unescaped
     (:pattern
      (cltpt/combinator:any
       (cltpt/combinator:consec
        (:pattern
         (cltpt/combinator:literal-casein "#+begin_src")
         :id open-tag)
        (cltpt/combinator:literal " ")
        (:pattern
         (cltpt/combinator:symbol-matcher)
         :id lang)
        (cltpt/combinator:literal " ")
        ,*keywords-rule*)
       (cltpt/combinator:consec
        (:pattern
         (cltpt/combinator:literal-casein "#+begin_src")
         :id open-tag)
        (cltpt/combinator:literal " ")
        (:pattern
         (cltpt/combinator:symbol-matcher)
         :id lang)))
      :id begin))
    (cltpt/combinator:unescaped
     (:pattern (cltpt/combinator:literal-casein "#+end_src")
      :id end))
    ;; unlike an `org-block', org-src-block shouldnt contain children (for now)
    nil))
(defvar *org-src-block-rule*
  `(:pattern
    (cltpt/combinator:any
     ;; block with keywords and execution results
     (cltpt/combinator:consec
      ,(copy-rule *org-keyword-rule* 'org-keyword)
      ,*org-src-block-no-kw-rule*
      (cltpt/combinator:literal ,(string #\newline))
      (cltpt/combinator:literal ,(string #\newline))
      ,*org-babel-results-rule*)
     ;; block with keywords but no execution results
     (cltpt/combinator:consec
      ,(copy-rule *org-keyword-rule* 'org-keyword)
      ,*org-src-block-no-kw-rule*)
     ;; block with results but no keywords
     (cltpt/combinator:consec
      ,*org-src-block-no-kw-rule*
      (cltpt/combinator:literal ,(string #\newline))
      (cltpt/combinator:literal ,(string #\newline))
      ,*org-babel-results-rule*)
     ;; block with no keywords and no results
     ,*org-src-block-no-kw-rule*)
    :on-char #\#))
(defclass org-src-block (cltpt/base:text-object)
  ((cltpt/base::rule
    :allocation :class
    :initform *org-src-block-rule*))
  (:documentation "org-mode src block."))

(defun init-org-src-block (obj match)
  (let* ((begin-match (car (cltpt/combinator:find-submatch match 'begin)))
         (end-match (car (cltpt/combinator:find-submatch match 'end)))
         (keywords-match (cltpt/combinator:find-submatch match 'keywords)))
    (setf (cltpt/base:text-object-property obj :contents-region)
          (cltpt/base:make-region :begin (- (getf begin-match :end)
                                            (getf begin-match :begin))
                                  :end (- (getf end-match :begin)
                                          (getf begin-match :begin))))
    (handle-block-keywords obj)))

(defmethod cltpt/base:text-object-init :after ((obj org-src-block) str1 match)
  (init-org-src-block obj match))

;; TODO: handle verbatim blocks such as #+begin_example.
(defmethod convert-block ((obj cltpt/base:text-object)
                          (backend cltpt/base:text-format)
                          block-type
                          is-code)
  (let* ((exports-keyword (org-block-keyword-value obj "exports"))
         (export-code (or (string= exports-keyword "code")
                          (string= exports-keyword "both")))
         (export-results (or (string= exports-keyword "results")
                             (string= exports-keyword "both")))
         (match (cltpt/base:text-object-property
                 obj
                 :combinator-match))
         ;; TODO: code-end-match is really just block-end-match, should be renamed
         ;; when its a regular block, we want to find the last end delimiter
         ;; in this match because there could be nested blocks that have
         ;; their own end delimiters. when its a code block we only
         ;; want the first since there cant be nested code blocks, and we dont
         ;; wanna catch the delimiters inside the results regions (if any)
         (code-end-match
           (if is-code
               (cltpt/combinator:find-submatch
                match
                'end)
               (cltpt/combinator:find-submatch-last
                match
                'end)))
         (results-match
           (cltpt/combinator:find-submatch
            match
            'results)))
    ;; export "both" by default, if :exports wasnt provided.
    ;; TODO: this shouldnt be the default behavior. we should have it customizable.
    (when (and is-code (not exports-keyword))
      (setf export-code t)
      (setf export-results t))
    (cond
      ;; if we have `:exports none', we shouldnt export
      ((and exports-keyword (string= exports-keyword "none"))
       (list :text "" :reparse t))
      ((member block-type (list "comment" "my_comment") :test 'string=)
       (list :text "" :reparse t))
      (t
       (let* ((all-keywords
                (concatenate
                 'list
                 `(,(cons "type" (cltpt/base:text-object-property obj :type)))
                 (cltpt/base:text-object-property obj :keywords-alist)))
              (props
                (loop for (key . value) in all-keywords
                      for result = (unless (member key '("exports" "results"))
                                     (format nil
                                             "data-~A='~A'"
                                             key
                                             value))
                      when result collect result))
              (props-str (cltpt/base:str-join props " "))
              (code-open-tag
                (cltpt/base:pcase backend
                  (cltpt/html:*html*
                   (if is-code
                       (format nil
                               "<pre class='org-src' ~A><code>"
                               props-str)
                       (format nil
                               "<div class='~A org-block' ~A>"
                               block-type
                               props-str)))
                  (cltpt/latex:*latex*
                   (format nil
                           "\\begin{~A}"
                           (if is-code
                               cltpt/latex:*latex-code-env*
                               block-type)))))
              (code-close-tag
                (cltpt/base:pcase backend
                  (cltpt/html:*html*
                   (if is-code
                       "</code></pre>"
                       "</div>"))
                  (cltpt/latex:*latex*
                   (format nil
                           "\\end{~A}"
                           (if is-code
                               cltpt/latex:*latex-code-env*
                               block-type)))))
              (changes)
              (results-open-tag
                (cltpt/base:pcase backend
                  (cltpt/html:*html* "<div class='org-babel-results'>")
                  (cltpt/latex:*latex*
                   (format nil
                           "\\begin{~A}"
                           cltpt/latex:*latex-code-env*))))
              (results-close-tag
                (cltpt/base:pcase backend
                  (cltpt/html:*html* "</div>")
                  (cltpt/latex:*latex*
                   (format nil
                           "\\end{~A}"
                           cltpt/latex:*latex-code-env*))))
              (escape-regions))
         ;; if we wanna export results, we will have to apply some modifications
         ;; to some of the text accordingly so that the #+RESULTS part
         ;; is ommitted and so that the children after it in the results
         ;; block are handled correctly (although it code be "raw" and
         ;; contain no children).
         ;; if its not code, we surround the block's contents with tags
         ;; and convert it.
         (when (and is-code export-results results-match)
           (let* (;; lines starting with ": "
                  (match-raw-lines
                    (cltpt/combinator:find-submatch-all
                     results-match
                     'output-line))
                  (results-content-match
                    (cltpt/combinator:find-submatch
                     results-match
                     'results-content))
                  (results-text)
                  (results-begin
                    (- (getf (car results-match) :begin)
                       (getf (car match) :begin)))
                  (results-end
                    (- (getf (car results-match) :end)
                       (getf (car match) :begin)))
                  (results-content-begin
                    (- (getf (car results-content-match) :begin)
                       (getf (car match) :begin)))
                  ;; code-*-tag-region is for the regions related
                  ;; to the code block /before/ the incremental changes.
                  (code-open-tag-region
                    (cltpt/base:make-region
                     :begin 0
                     :end (cltpt/base:region-begin
                           (cltpt/base:text-object-contents-region obj))))
                  (code-close-tag-region
                    (cltpt/base:make-region
                     :begin (cltpt/base:region-end
                             (cltpt/base:text-object-contents-region
                              obj))
                     :end (- (getf (car code-end-match) :end)
                             (getf (car match) :begin))))
                  ;; those regions are also /before/ incremental changes
                  (results-content-region
                    (cltpt/base:make-region
                     :begin results-content-begin
                     :end results-end))
                  (results-region
                    (cltpt/base:make-region
                     :begin results-begin
                     :end results-end))
                  (code-region
                    (cltpt/base:make-region
                     :begin (cltpt/base:region-end code-open-tag-region)
                     :end (cltpt/base:region-begin code-close-tag-region)))
                  (raw-results
                    (when match-raw-lines
                      (cltpt/base:str-join
                       (mapcar
                        (lambda (raw-line-match)
                          (subseq
                           (cltpt/combinator:match-text
                            (car raw-line-match))
                           2))
                        match-raw-lines)
                       (string #\newline)))))
             ;; both regions of results and code need escaping.
             ;; but the region for the code shouldnt have the newlines escaped.
             ;; but we need to account for the changes in the preceding
             ;; region because :escape-regions is applied after the
             ;; incremental changes are applied.
             (push
              (cltpt/base:make-region
               :begin results-content-begin
               :end results-end)
              escape-regions)
             (push (cons code-region (list :escape-newlines nil))
                   escape-regions)
             ;; we have to push the changes in the correct order. otherwise
             ;; the incremental parser will not function properly.
             ;; changes in the results region
             (push
              (cons results-close-tag
                    (cltpt/base:make-region
                     :begin results-end
                     :end results-end))
              changes)
             (when match-raw-lines
               ;; if its the raw lines (": ") we need to convert, just subseq
               ;; them accordingly to get rid of the colon and space.
               ;; in the other case, its just a collection of org elements
               ;; so we just need to convert them all in the given text region
               ;; of the "results".
               (push (cons raw-results results-content-region) changes))
             (push
              (cons results-open-tag
                    (cltpt/base:make-region
                     :begin results-begin
                     :end results-content-begin))
              changes)
             ;; changes in the code block region
             (push (cons code-close-tag code-close-tag-region) changes)
             (push (cons code-open-tag code-open-tag-region) changes)))
         (if changes
             (list :text (cltpt/base:text-object-text obj)
                   :changes changes
                   :recurse t
                   :escape-regions escape-regions
                   :escape t
                   :reparse nil)
             (cltpt/base:rewrap-within-tags
              obj
              code-open-tag
              code-close-tag
              :escape t
              :compress-region (cltpt/base:make-region :begin 1 :end 1)
              :escape-region-options (when is-code
                                       (list :escape-newlines nil)))))))))

(defmethod handle-block-keywords ((obj cltpt/base:text-object))
  (let* ((match (cltpt/base:text-object-property obj :combinator-match))
         (entries
           (cltpt/combinator:find-submatch-all
            (cltpt/combinator:find-submatch match 'keywords)
            'keywords-entry)))
    (loop for entry in entries
          for kw-match = (car (cltpt/combinator:find-submatch entry 'keyword))
          for val-match = (car (cltpt/combinator:find-submatch entry 'value))
          for kw = (cltpt/combinator:match-text kw-match)
          for val = (cltpt/combinator:match-text val-match)
          do (push (cons kw val) (cltpt/base:text-object-property obj :keywords-alist)))))

(defmethod cltpt/base:text-object-convert ((obj org-src-block)
                                           (backend cltpt/base:text-format))
  (convert-block obj backend nil t))

(defvar *org-block-no-kw-rule*
  `(cltpt/combinator:pair
    (cltpt/combinator:unescaped
     (:pattern
      (cltpt/combinator:any
       (cltpt/combinator:consec
        (cltpt/combinator::unsucceeded-by
         (cltpt/combinator:literal-casein "#+begin_")
         (cltpt/combinator:literal-casein "src"))
        (:pattern (cltpt/combinator:symbol-matcher)
         :id begin-type)
        (cltpt/combinator:literal " ")
        ,*keywords-rule*)
       (cltpt/combinator:consec
        (cltpt/combinator::unsucceeded-by
        (cltpt/combinator:literal-casein "#+begin_")
        (cltpt/combinator:literal-casein "src"))
        (:pattern (cltpt/combinator:symbol-matcher)
         :id begin-type)))
      :id begin))
    (cltpt/combinator:unescaped
     (:pattern
      (cltpt/combinator:consec
       (cltpt/combinator::unsucceeded-by
        (cltpt/combinator:literal-casein "#+end_")
        (cltpt/combinator:literal-casein "src"))
       (:pattern (cltpt/combinator:symbol-matcher)
        :id end-type))
      :id end))
    ;; an org-block can contain every other object except headers
    (eval
     (org-mode-text-object-types-except '(org-header)))))
(defvar *org-block-rule*
  `(:pattern
    (cltpt/combinator:any
     (cltpt/combinator:consec
      ,(copy-rule *org-keyword-rule* 'org-keyword)
      ,*org-block-no-kw-rule*)
     ,*org-block-no-kw-rule*)
    :on-char #\#))
(defclass org-block (cltpt/base:text-object)
  ((cltpt/base::rule
    :allocation :class
    :initform *org-block-rule*))
  (:documentation "org-mode block."))

(defmethod org-block-keyword-value ((obj cltpt/base:text-object) kw)
  "return the value associated with a keyword of an `org-block'."
  (cdr (assoc kw
              (cltpt/base:text-object-property obj :keywords-alist)
              :test 'equal)))

(defmethod cltpt/base:text-object-init :after ((obj org-block) str1 match)
  ;; grab the "type" of the block, set content boundaries, need to grab keywords
  (let* ((begin-type-match (car (cltpt/combinator:find-submatch match 'begin-type)))
         (begin-type (cltpt/combinator:match-text begin-type-match))
         (begin-match (car (cltpt/combinator:find-submatch match 'begin)))
         ;; we look for the last instance of 'end because otherwise
         ;; we might capture the 'end of another block nested
         ;; within this one
         (end-match (car (cltpt/combinator:find-submatch-last match 'end))))
    (setf (cltpt/base:text-object-property obj :type) begin-type)
    (setf (cltpt/base:text-object-property obj :contents-region)
          (cltpt/base:make-region :begin (- (getf begin-match :end)
                                            (getf begin-match :begin))
                                  :end (- (getf end-match :begin)
                                          (getf begin-match :begin))))
    ;; handle keywords
    (handle-block-keywords obj)
    (let ((block-title (org-block-keyword-value obj "title"))
          (block-name (org-block-keyword-value obj "name")))
      ;; handle :name <id> as an anchor for blocks. construct a roam node for each.
      (when block-name
        (setf (cltpt/base:text-object-property obj :roam-node)
              (cltpt/roam:make-node
               :id block-name
               :title block-title
               :desc nil
               :text-obj obj))))))

(defmethod cltpt/base:text-object-convert ((obj org-block)
                                           (backend cltpt/base:text-format))
  (convert-block obj backend (cltpt/base:text-object-property obj :type) nil))

;; an "export block" is very similar to an src-block, just some slight differences.
(defvar *org-export-block-rule*
  `(:pattern
    ,(copy-modify-rule
      *org-src-block-no-kw-rule*
      '((open-tag . "#+begin_export")
        (end . "#+end_export")))
    :id org-export-block
    :on-char #\#))
(defclass org-export-block (cltpt/base:text-object)
  ((cltpt/base::rule
    :allocation :class
    :initform *org-export-block-rule*))
  (:documentation "org-mode export block."))

(defmethod cltpt/base:text-object-init :after ((obj org-export-block) str1 match)
  (init-org-src-block obj match))

(defmethod cltpt/base:text-object-convert ((obj org-export-block)
                                           (backend cltpt/base:text-format))
  (let* ((match (cltpt/base:text-object-property obj :combinator-match))
         (lang-match (car (cltpt/combinator:find-submatch match 'lang)))
         (lang (cltpt/combinator:match-text lang-match)))
    ;; we only export if the destination matches the lang set by the export block
    (if (string= (cltpt/base:text-format-name backend) lang)
        (list :text (cltpt/base:text-object-contents obj)
              :recurse nil
              :reparse nil
              :escape nil)
        (list :text ""
              :reparse t))))

(defvar *org-drawer-rule*
  `(:pattern
    (cltpt/combinator:pair
     (cltpt/combinator:followed-by
      (:pattern ,(cltpt/combinator:handle-rule-string ":%w:")
       :id drawer-open-tag)
      cltpt/combinator:at-line-end-p)
     (cltpt/combinator:when-match
      (cltpt/combinator:literal-casein ":end:")
      cltpt/combinator:at-line-start-p)
     (eval (org-mode-text-object-types-except '(org-header org-drawer))))
    :on-char #\:))
(defclass org-drawer (cltpt/base:text-object)
  ((cltpt/base::rule
    :allocation :class
    :initform *org-drawer-rule*))
  (:documentation "org-mode drawer."))

;; we need an org-specific flavor of latex-env that may accept keywords like #+name
(defvar *org-latex-env-rule*
  `(:pattern
    ,(rule-with-org-keywords
      (copy-rule
       cltpt/latex:*latex-env-rule*
       ;; we cant use an id 'latex-env' because that will cause a child of type
       ;; cltpt/latex:latex-env to be matched. we use 'latex-env-1'
       'latex-env-1)
      t)
    :on-char #\#))
(defclass org-latex-env (cltpt/latex:latex-env)
  ((cltpt/base::rule
    :allocation :class
    :initform *org-latex-env-rule*))
  (:documentation "type for org-mode-specific latex environments (latex-env that may be preceded with keywords like #+name)."))

(defmethod cltpt/base:text-object-finalize ((obj org-latex-env))
  (let* ((keywords-alist (handle-parsed-org-keywords obj))
         (name (cltpt/base:alist-get keywords-alist "name"))
         (caption (cltpt/base:alist-get keywords-alist "caption")))
    (setf (cltpt/base:text-object-property obj :roam-node)
          (cltpt/roam:make-node :id name
                                :title nil
                                :desc nil
                                :text-obj obj))))

(defun region-in-tags (match &optional include-tags)
  "takes a combinator match, returns a `region' that should contain the contents between the begin/end tags, like the indicies for the region enclosing \\begin{tag}..\\end{tag} for latex environments.

according to INCLUDE-TAGS we decide whether the region should enclose the tags themselves or not."
  ;; we're using 'open-tag here which may not precisely equal 'cltpt/latex::open-tag
  ;; but it is fine since find-submatch uses 'string=
  (let* ((begin-match (car (cltpt/combinator:find-submatch match 'open-tag)))
         (end-match (car (cltpt/combinator:find-submatch-last match 'close-tag)))
         (absolute-begin (getf (car match) :begin)))
    (if include-tags
        (cltpt/base:make-region
         :begin (- (getf begin-match :begin) absolute-begin)
         :end (- (getf end-match :end) absolute-begin))
        (cltpt/base:make-region
         :begin (- (getf begin-match :end)
                   (getf begin-match :begin))
         :end (- (getf end-match :begin)
                 (getf begin-match :begin))))))

(defmethod cltpt/base:text-object-init :after ((obj org-latex-env) str1 match)
  (setf (cltpt/base:text-object-property obj :contents-region)
        (region-in-tags match t)))