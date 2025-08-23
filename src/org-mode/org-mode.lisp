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
   :*org-mode* :org-mode-text-object-types
   :org-block :init :*org-enable-macros*))

(in-package :cltpt/org-mode)

(defvar *org-enable-macros* nil)

(defun init ()
  (setf (cltpt/base:text-format-text-object-types *org-mode*)
        (concatenate
         'list
         '(org-list
           org-table
           org-keyword
           org-header
           org-link
           org-src-block
           org-export-block
           org-block
           org-prop-drawer
           org-drawer
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
  (setf (cltpt/base:text-format-name *org-mode*)
        "org-mode")
  (setf (cltpt/base:text-format-document-type *org-mode*)
        'org-document))

(defvar *org-mode*
  (make-text-format "org-mode")
  "`text-format' instance of the org-mode format.")

(defvar *org-document-convert-with-boilerplate*
  t
  "whether to convert with preamble/postamble.")

(defun org-mode-text-object-types ()
  (cltpt/base:text-format-text-object-types *org-mode*))

(defun org-mode-inline-text-object-types ()
  (intersection
   '(org-link web-link org-inline-code org-emph org-italic
     cltpt/latex:display-math cltpt/latex:inline-math cltpt/latex:latex-env)
   (org-mode-text-object-types)))

(defun copy-rule-with-id (rule id)
  (if (cltpt/base::plistp rule)
      (let ((copy (copy-tree rule)))
        (setf (getf copy :id) id)
        copy)
      (list :pattern (copy-tree rule) :id id)))

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
     (copy-rule-with-id
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
                             (getf (car key) :match)
                             (getf (car val) :match)))))

;; simply dont convert drawers (this isnt the correct org-mode behavior tho)
(defmethod cltpt/base:text-object-convert ((obj org-prop-drawer) backend)
  "")

(defvar *org-keyword-rule*
  '(:pattern
    (cltpt/combinator:consec
     (cltpt/combinator:literal "#+")
     (:pattern (cltpt/combinator:symbol-matcher)
      :id keyword)
     (cltpt/combinator:literal ":")
     (cltpt/combinator:atleast-one-discard (cltpt/combinator:literal " "))
     (:pattern (cltpt/combinator:all-but-newline)
      :id value))
    :on-char #\#))
(defclass org-keyword (cltpt/base:text-object)
  ((cltpt/base::rule
    :allocation :class
    :initform *org-keyword-rule*))
  (:documentation "org-mode file-level keyword."))

(defmethod cltpt/base:text-object-init :after ((obj org-keyword) str1 match)
  (let* ((value-match (car (cltpt/combinator:find-submatch match 'value)))
         (keyword-match (car (cltpt/combinator:find-submatch match 'keyword))))
    (setf (cltpt/base:text-object-property obj :value)
          (getf value-match :match))
    (setf (cltpt/base:text-object-property obj :keyword)
          (getf keyword-match :match))))

(defmethod cltpt/base:text-object-convert ((obj org-keyword) backend)
  (let* ((kw (cltpt/base:text-object-property obj :keyword))
         (value (cltpt/base:text-object-property obj :value)))
    ;; handle transclusions
    (if (and kw (string= kw "transclude") cltpt/roam:*roam-convert-data*)
        (let* ((org-link-parse-result (cltpt/base:parse value '(org-link)))
               (first-child (car (cltpt/base:text-object-children
                                  org-link-parse-result))))
          (when (typep first-child 'org-link)
            (let* ((dest (cltpt/base:text-object-property first-child :dest))
                   (type (cltpt/base:text-object-property first-child :type))
                   (rmr (getf cltpt/roam:*roam-convert-data* :roamer))
                   (roam-link (cltpt/roam:resolve-link
                               rmr
                               (getf cltpt/roam:*roam-convert-data* :node)
                               obj
                               ;; if link doesnt have a type, treat it as an 'id' link
                               (if type
                                   (intern type :cltpt/roam)
                                   'cltpt/roam::id)
                               dest)))
              ;; just convert the linked node's object and return that
              (when roam-link
                (let* ((dest-node (cltpt/roam:link-dest-node roam-link))
                       (dest-text-obj (cltpt/roam:node-text-obj dest-node)))
                  (let ((*org-document-convert-with-boilerplate*))
                    (cltpt/base:text-object-convert dest-text-obj backend)))))))
        (list :remove-newlines-after t))))

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

(defmethod cltpt/base:text-object-convert ((obj org-comment) backend)
  (list :remove-newlines-after t))

(defvar *org-timestamp-rule*
  '(:pattern
    (cltpt/combinator:consec
     "<"
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
     ">")
    :on-char #\<))
(defclass org-timestamp (cltpt/base:text-object)
  ((cltpt/base::rule
    :allocation :class
    :initform *org-timestamp-rule*))
  (:documentation "a timestamp/date. e.g. <2023-12-28 Thu 18:30:00>."))

(defun org-timestamp-match-to-time (match)
  (let* ((day (parse-integer
               (getf (car (cltpt/combinator:find-submatch match 'day)) :match)
               :junk-allowed t))
         (second-str (getf (car (cltpt/combinator:find-submatch match 'second)) :match))
         (second (when second-str (parse-integer second-str :junk-allowed t)))
         (year (parse-integer
                (getf (car (cltpt/combinator:find-submatch match 'year)) :match)
                :junk-allowed t))
         (month (parse-integer
                 (getf (car (cltpt/combinator:find-submatch match 'month)) :match)
                 :junk-allowed t))
         (hour (parse-integer
                (getf (car (cltpt/combinator:find-submatch match 'hour)) :match)
                :junk-allowed t))
         (minute (parse-integer
                  (getf (car (cltpt/combinator:find-submatch match 'minute)) :match)
                  :junk-allowed t))
         (weekday (car (cltpt/combinator:find-submatch match 'weekday))))
    ;; **encode-timestamp** nsec sec minute hour day month year &key timezone offset into
    (local-time:encode-timestamp 0
                                 (or second 0)
                                 minute
                                 hour
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
(defmethod cltpt/base:text-object-convert ((obj org-list) backend)
  ;; create a new non-org-list object, export it, use the output as a new list
  ;; reparse, that new list, then export the modified newly parsed list as if
  ;; it was the original
  (let* ((list-text (cltpt/base:text-object-text obj))
         (new-children (cltpt/base:text-object-children
                        (cltpt/base:parse list-text
                                          (org-mode-inline-text-object-types))))
         (new-obj (make-instance 'cltpt/base:text-object)))
    (cltpt/base:text-object-init
     new-obj
     list-text
     (cltpt/base:text-object-property obj :combinator-match))
    ;; set children of new-obj to those of obj without any nested org-lists
    ;; otherwise things wont work properly (because nested org-lists get converted)
    ;; to latex and later we try to parse them as a list
    (setf (cltpt/base:text-object-children new-obj)
          new-children)
    ;; we create a new intermediate object, treat it as raw text,
    ;; parse other types of text objects and convert them, then parse the result
    ;; as a list
    (let* ((new-txt (cltpt/base:convert-tree new-obj
                                             backend
                                             (org-mode-inline-text-object-types)
                                             nil
                                             t
                                             nil))
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

(defvar *org-header-rule*
  `(:pattern
    (cltpt/combinator:consec-atleast-one
     (cltpt/combinator:when-match
      (cltpt/combinator:any
       (cltpt/combinator:consec
        (:pattern
         (cltpt/combinator:atleast-one-discard (cltpt/combinator:literal "*"))
         :id stars)
        (cltpt/combinator:atleast-one-discard (cltpt/combinator:literal " "))
        (:pattern (cltpt/combinator:upcase-word-matcher)
         :id todo-keyword)
        (cltpt/combinator:atleast-one-discard (cltpt/combinator:literal " "))
        (:pattern (cltpt/combinator:all-but-newline)
         :id title))
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
            ,(copy-rule-with-id *org-timestamp-rule* 'timestamp))
           :id action-active)
          (:pattern
           (cltpt/combinator:consec
            (:pattern (cltpt/combinator:upcase-word-matcher)
             :id name)
            ": "
            ,(copy-rule-with-id *org-timestamp-bracket-rule* 'timestamp))
           :id action-inactive)))
        ,(copy-rule-with-id *org-timestamp-rule* 'todo-timestamp)
        ,(copy-rule-with-id *org-list-rule* 'org-list)
        ,(copy-rule-with-id *org-prop-drawer-rule* 'org-prop-drawer)))))
    :on-char #\*))
(defclass org-header (cltpt/base:text-object)
  ((cltpt/base::rule
    :allocation :class
    :initform *org-header-rule*))
  (:documentation "org-mode header."))

(defmethod cltpt/base:text-object-init :after ((obj org-header) str1 match)
  (let* ((stars (car (cltpt/combinator:find-submatch match 'stars)))
         (title (car (cltpt/combinator:find-submatch match 'title))))
    (setf (cltpt/base:text-object-property obj :level)
          (length (getf stars :match)))
    (setf (cltpt/base:text-object-property obj :title)
          (getf title :match))))

(defmethod cltpt/base:text-object-finalize ((obj org-header))
  ;; initialize agenda data
  (let* ((match (cltpt/base:text-object-property obj :combinator-match))
         (title-match (car (cltpt/combinator:find-submatch match 'title)))
         (header-id)
         (scheduled)
         (deadline)
         (todo-keyword-match (car (cltpt/combinator:find-submatch match 'todo-keyword)))
         (timestamp-match (cltpt/combinator:find-submatch match 'todo-timestamp))
         (action-active-matches (cltpt/combinator:find-submatch-all match 'action-active))
         (action-inactive-matches (cltpt/combinator:find-submatch-all match 'action-inactive))
         (todo-timestamp (when timestamp-match
                           (org-timestamp-match-to-time timestamp-match))))
    (labels ((is-drawer (obj2)
               (typep obj2 'org-prop-drawer)))
      (let ((drawers (find-children obj #'is-drawer)))
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
          do (let ((action-name (getf (car (cltpt/combinator:find-submatch action-match 'name)) :match))
                   (action-timestamp (cltpt/combinator:find-submatch action-match 'timestamp)))
               (cond
                 ((string-equal action-name "scheduled")
                  (setf scheduled (org-timestamp-match-to-time action-timestamp)))
                 ((string-equal action-name "deadline")
                  (setf deadline (org-timestamp-match-to-time action-timestamp)))
                 ((string-equal action-name "closed")
                  ;; TODO
                  ))))
    (setf (text-object-property obj :id) header-id)
    ;; initialize roam data
    (setf (cltpt/base:text-object-property obj :roam-node)
          (cltpt/roam:make-node
           :id header-id
           :title (getf title-match :match)
           :desc nil
           :text-obj obj))
    (when todo-keyword-match
      (setf (cltpt/base:text-object-property obj :todo)
            (cltpt/agenda:make-todo
             :title (getf title-match :match)
             :description nil
             :state (getf todo-keyword-match :match)
             :scheduled scheduled
             :deadline deadline
             :timed todo-timestamp
             :tags nil
             :state-history nil)))))

(defmethod cltpt/base:text-object-convert ((obj org-header) backend)
  (cltpt/base:pcase backend
    (cltpt/latex:*latex*
     (let* ((begin-text
              (format
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
    (cltpt/html:*html*
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
             :recurse t
             :remove-newlines-after t)))))

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
(defclass org-link (cltpt/base:text-object)
  ((cltpt/base::shared-name
    :allocation :class
    :initform 'cltpt/base::link)
   (cltpt/base::rule
    :allocation :class
    :initform *org-link-rule*))
  (:documentation "org-mode link."))

(defmethod cltpt/base:text-object-convert ((obj org-link) backend)
  (cond
    ((eq backend cltpt/html:*html*)
     (let* ((desc (cltpt/base:text-object-property obj :desc))
            (dest (cltpt/base:text-object-property obj :dest))
            (type (cltpt/base:text-object-property obj :type))
            (final-desc (or desc dest)))
       (when dest
         (if cltpt/roam:*roam-convert-data*
             (cltpt/roam:convert-link
              (getf cltpt/roam:*roam-convert-data* :roamer)
              (getf cltpt/roam:*roam-convert-data* :node)
              obj
              (getf cltpt/roam:*roam-convert-data* :filepath-format)
              backend)
             (within-tags
              (if cltpt/html:*html-static-route*
                  (format nil "<a href='~A'>"
                          (cltpt/base:change-dir dest cltpt/html:*html-static-route*))
                  (format nil "<a href='~A'>" dest))
              final-desc
              "</a>")))))))

(defmethod cltpt/base:text-object-init :after ((obj org-link) str1 match)
  (let ((link-type-match (car (cltpt/combinator:find-submatch match 'link-type)))
        (link-dest-match (car (cltpt/combinator:find-submatch match 'link-dest)))
        (link-desc-match (car (cltpt/combinator:find-submatch match 'link-desc))))
    (setf (cltpt/base:text-object-property obj :desc)
          (getf link-desc-match :match))
    (setf (cltpt/base:text-object-property obj :dest)
          (getf link-dest-match :match))
    (setf (cltpt/base:text-object-property obj :type)
          (getf link-type-match :match))))

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

(defmethod cltpt/base:text-object-convert ((obj web-link) backend)
  (cond
    ((eq backend cltpt/html:*html*)
     (format nil "<a href='~A'></a>" (cltpt/base:text-object-text obj)))))

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

(defmethod cltpt/base:text-object-finalize ((obj org-inline-code))
  (compress-contents-region-by-one obj))

(defmethod cltpt/base:text-object-convert ((obj org-inline-code) backend)
  (cond
    ((eq backend cltpt/latex:*latex*)
     (let ((result (cltpt/base:wrap-contents-for-convert obj "\\verb{" "}")))
       (setf (getf result :reparse-region) nil)
       result))
    ((eq backend cltpt/html:*html*)
     (cltpt/base:wrap-contents-for-convert obj "<code>" "</code>"))))

(defclass org-table (cltpt/base:text-object)
  ((cltpt/base::rule
    :allocation :class
    :initform `(:pattern
                (org-table-matcher
                 ,*org-inline-text-objects-rule*)
                :on-char #\|)))
  (:documentation "org-mode table."))

;; hacky, like the one for org-list, they need to be improved
(defmethod cltpt/base:text-object-convert ((obj org-table) backend)
  (let ((new-obj (make-instance 'cltpt/base:text-object)))
    (cltpt/base:text-object-init
     new-obj
     (cltpt/base:text-object-text obj)
     ;; TODO: shouldnt we passing the combinator context from the original
     ;; matching run? this could cause issues
     (org-table-matcher nil (cltpt/base:text-object-text obj) 0))
    (setf (cltpt/base:text-object-children new-obj)
          (cltpt/base:text-object-children obj))
    (let* ((new-txt (cltpt/base:convert-tree
                     new-obj
                     backend
                     (org-mode-inline-text-object-types)
                     nil
                     t
                     nil))
           (parsed (org-table-matcher nil new-txt 0)))
      (cond
        ((eq backend cltpt/latex:*latex*)
         (list :text (to-latex-table parsed)
               :recurse nil
               :reparse nil
               :escape nil))
        ((eq backend cltpt/html:*html*)
         (list :text (to-html-table parsed)
               :recurse nil
               :reparse nil
               :escape nil))))))

(defclass org-document (cltpt/base:document)
  ()
  (:documentation "org-mode document."))

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
    (loop for child in (if first-child-is-drawer
                           (cdr (cltpt/base:text-object-children obj))
                           (cltpt/base:text-object-children obj))
          while (typep child 'org-keyword)
          do (let ((kw-name (cltpt/base:text-object-property child :keyword))
                   (kw-value (cltpt/base:text-object-property child :value)))
               (when (string= kw-name "title")
                 (setf doc-title kw-value))
               ;; denote-style identifier
               (when (string= kw-name "identifier")
                 (setf doc-id kw-value))
               (when (string= kw-name "date")
                 (setf doc-date kw-value))
               (when (string= kw-name "filetags")
                 ;; avoid first and last ':', split by ':' to get tags
                 (setf doc-tags (str:substring 1 -1 kw-value)))))
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
    ;; TODO: properly set the hierarchy of objects in headings
    ;; currently headings dont *enclose* their children like they should
    ))

(defun ensure-latex-previews-generated (org-doc)
  (let ((mylist))
    (cltpt/base:map-text-object
     org-doc
     (lambda (obj)
       (when (or (typep obj 'cltpt/latex:inline-math)
                 (typep obj 'cltpt/latex:display-math)
                 (typep obj 'cltpt/latex:latex-env))
         (push (cltpt/base:text-object-text obj) mylist))))
    (cltpt/latex:generate-svgs-for-latex mylist)))

(defmethod cltpt/base:text-object-convert ((obj org-document) backend)
  (cltpt/base:pcase backend
    (cltpt/latex:*latex*
     (let* ((my-preamble
              (if *org-document-convert-with-boilerplate*
                  (concatenate 'string
                               (cltpt/latex:generate-latex-preamble
                                "authorhere" "datehere" "titlehere")
                               (string #\newline)
                               "\\begin{document}"
                               (string #\newline))
                  ""))
            (my-postamble
              (if *org-document-convert-with-boilerplate*
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
    (cltpt/html:*html*
     (let* ((my-preamble
              (concatenate
               'string
               (if *org-document-convert-with-boilerplate*
                   (cltpt/base:text-format-generate-preamble
                    cltpt/html:*html*
                    obj)
                   "")
               "<div class='content'>"))
            (my-postamble
              (concatenate
               'string
               "</div>"
               (if *org-document-convert-with-boilerplate*
                   (cltpt/base:text-format-generate-postamble
                    cltpt/html:*html*
                    obj)
                   "")))
            (my-text (concatenate 'string
                                  my-preamble
                                  (cltpt/base:text-object-text obj)
                                  my-postamble))
            (inner-region (cltpt/base:make-region
                           :begin (length my-preamble)
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
                :escapable #\*
                :on-char #\*)))
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
    ((eq backend cltpt/latex:*latex*)
     (let ((result (cltpt/base:wrap-contents-for-convert obj "\\textbf{" "}")))
       (setf (getf result :reparse-region) nil)
       result))
    ((eq backend cltpt/html:*html*)
     (cltpt/base:wrap-contents-for-convert obj "<b>" "</b>"))))

(defvar *org-italic-rule*
  '(:pattern
    (cltpt/combinator:pair
     (cltpt/combinator:unescaped (cltpt/combinator:literal "/"))
     (cltpt/combinator:unescaped (cltpt/combinator:literal "/"))
     nil
     nil
     nil)
    :on-char #\/))
(defclass org-italic (cltpt/base:text-object)
  ((cltpt/base::rule
    :allocation :class
    :initform *org-italic-rule*))
  (:documentation "org-mode italicized text (surrounded by forward slahes)."))

(defmethod cltpt/base:text-object-finalize ((obj org-italic))
  (compress-contents-region-by-one obj))

(defmethod cltpt/base:text-object-convert ((obj org-italic) backend)
  (cond
    ((eq backend cltpt/latex:*latex*)
     (let ((result (cltpt/base:wrap-contents-for-convert obj "\\textit{" "}")))
       (setf (getf result :reparse-region) nil)
       result))
    ((eq backend cltpt/html:*html*)
     (cltpt/base:wrap-contents-for-convert obj "<i>" "</i>"))))

;; wrap text for exporting within specific "tags".
(defun within-tags (open-tag inner-text close-tag)
  (let* ((text (concatenate 'string
                            open-tag
                            inner-text
                            close-tag)))
    (list :text text
          :recurse t
          :reparse-region (cltpt/base:make-region
                           :begin (length open-tag)
                           :end (- (length text) (length close-tag)))
          :reparse t)))

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
        (cltpt/combinator:succeeded-by
         (:pattern (cltpt/combinator:lisp-sexp)
          :id value)
         ;; we make sure to capture lisp expressions only if they are succeeded
         ;; by " :" because otherwise we might capture a word that is part
         ;; of a sequence of words, like ":title my title", which is a case
         ;; that should be handled by the following rule, not this one.
         (cltpt/combinator:literal " :")))
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
          (cltpt/combinator:all-upto
           (cltpt/combinator:any
            (cltpt/combinator:literal ,(string #\newline))
            (cltpt/combinator:literal " :")))
          :id value)))
       :id keywords-entry)))
    :id keywords))

;; TODO: make org-src-block contain #+results too
(defvar *org-babel-results-rule*
  `(cltpt/combinator:consec
    (cltpt/combinator:any
     (cltpt/combinator:literal-casein "#+results:")
     (cltpt/combinator:consec
      (cltpt/combinator:literal-casein "#+results[")
      (cltpt/combinator:symbol-matcher)
      (cltpt/combinator:literal "]:")))
    (cltpt/combinator:any
     #| detect syntax like
     #+RESULTS[c419e84a898b0cdd18b49e14c750b71743921b86]:
     : (5 1 7 4)
     : more text
     : more text
     |#
     (cltpt/combinator:atleast-one
      (cltpt/combinator:consec
       (cltpt/combinator:literal ,(string #\newline))
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
     (cltpt/combinator:consec
      (cltpt/combinator:literal ,(string #\newline))
      (cltpt/combinator:any
       ,(copy-rule-with-id *org-list-rule* 'org-list)
       org-table
       org-block
       org-drawer
       ,(copy-rule-with-id *org-link-rule* 'org-link))))))
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
      ,*org-keyword-rule*
      ,*org-src-block-no-kw-rule*
      (cltpt/combinator:literal ,(string #\newline))
      (cltpt/combinator:literal ,(string #\newline))
      ,*org-babel-results-rule*)
     ;; block with keywords but no execution results
     (cltpt/combinator:consec
      ,*org-keyword-rule*
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

;; placeholder
(defun to-raw-html-string (mystr)
  mystr)

;; should take an instance of `org-src-block' or `org-block', but that isnt ensured
(defmethod convert-block ((obj text-object) backend block-type is-code)
  (let ((exports-keyword
          (cdr (assoc "exports"
                      (text-object-property obj :keywords-alist)
                      :test 'equal)))
        (contents (cltpt/base:text-object-contents obj)))
    ;; when the contents start with the newline that is after the #+begin_block
    ;; it causes a weird newline when cltpt/base:*convert-escape-newlines* is `t'
    ;; so remove it.
    (when (char= (char contents 0) #\newline)
      (setf contents (subseq contents 1)))
    (cond
      ;; if we have `:exports none', we shouldnt export
      ((and exports-keyword (string= exports-keyword "none"))
       (list :text "" :reparse t))
      ((member block-type (list "comment" "my_comment") :test 'string=)
       (list :text "" :reparse t))
      ((eq backend cltpt/latex:*latex*)
       (let* ((begin-tag (format nil "\\begin{~A}" block-type))
              (end-tag (format nil "\\end{~A}" block-type))
              (my-text (concatenate 'string
                                    begin-tag
                                    contents
                                    end-tag))
              (inner-region
                (cltpt/base:make-region
                 :begin (length begin-tag)
                 :end (- (length my-text) (length end-tag)))))
         (list :text my-text
               :reparse (not is-code)
               :escape (not is-code)
               :reparse-region inner-region
               :escape-region inner-region)))
      ((eq backend cltpt/html:*html*)
       (let* ((props
                (loop for (key . value)
                        in (cltpt/base:text-object-property obj :keywords-alist)
                      for result = (unless (member key '("exports" "results"))
                                     (format nil
                                             "data-~A='~A'"
                                             key
                                             (to-raw-html-string value)))
                      when result collect result))
              (open-tag
                (if is-code
                    "<pre class='org-src'><code>"
                    (format nil
                            "<div class='~A org-block' ~A>"
                            block-type
                            (str:join " " props))))
              (close-tag (if is-code
                             "</code></pre>"
                             "</div>"))
              (text (concatenate 'string
                                 open-tag
                                 contents
                                 close-tag))
              (inner-region (cltpt/base:make-region
                             :begin (length open-tag)
                             :end (- (length text) (length close-tag)))))
         (list :text text
               :recurse (not is-code)
               :reparse (not is-code)
               :escape (not is-code)
               :reparse-region inner-region
               :escape-region inner-region
               :remove-newlines-after t))))))

(defmethod handle-block-keywords ((obj text-object))
  (let* ((match (cltpt/base:text-object-property obj :combinator-match))
         (entries
           (cltpt/combinator:find-submatch-all
            (cltpt/combinator:find-submatch match 'keywords)
            'keywords-entry)))
    (loop for entry in entries
          for kw-match = (car (cltpt/combinator:find-submatch entry 'keyword))
          for val-match = (car (cltpt/combinator:find-submatch entry 'value))
          for kw = (getf kw-match :match)
          for val = (getf val-match :match)
          do (push (cons kw val) (text-object-property obj :keywords-alist)))))

(defmethod cltpt/base:text-object-convert ((obj org-src-block) backend)
  (convert-block obj backend "lstlisting" t))

;; copied from older code needs to be rewritten.
(defmethod convert-babel-results ((obj org-src-block) backend)
  (let* ((match (cltpt/base:text-object-property obj :combinator-match))
         (output-line-matches (cltpt/combinator:find-submatch-all match 'output-line))
         (output-text (str:join
                       (string #\newline)
                       (mapcar
                        (lambda (output-line-match)
                          (subseq (getf (car output-line-match) :match) 2))
                        output-line-matches))))
    (cond
      ((eq backend cltpt/latex:*latex*)
       (list :text output-text
             :recurse nil
             :reparse nil
             :escape nil))
      ((eq backend cltpt/html:*html*)
       (concatenate
        'list
        (list :remove-newlines-after t)
        (within-tags "<code class='org-babel-results'><pre>"
                     output-text
                     "</code></pre>"))))))

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
      ,*org-keyword-rule*
      ,*org-block-no-kw-rule*)
     ,*org-block-no-kw-rule*)
    :on-char #\#))
(defclass org-block (cltpt/base:text-object)
  ((cltpt/base::rule
    :allocation :class
    :initform *org-block-rule*))
  (:documentation "org-mode block."))

(defmethod org-block-keyword-value ((obj org-block) kw)
  "return the value associated with a keyword of an `org-block'."
  (cdr (assoc kw
              (text-object-property obj :keywords-alist)
              :test 'equal)))

(defmethod cltpt/base:text-object-init :after ((obj org-block) str1 match)
  ;; grab the "type" of the block, set content boundaries, need to grab keywords
  (let* ((begin-type-match (car (cltpt/combinator:find-submatch match 'begin-type)))
         (begin-type (getf begin-type-match :match))
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

(defmethod cltpt/base:text-object-convert ((obj org-block) backend)
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

(defmethod cltpt/base:text-object-convert ((obj org-export-block) backend)
  (let* ((match (cltpt/base:text-object-property obj :combinator-match))
         (lang-match (car (cltpt/combinator:find-submatch match 'lang)))
         (lang (getf lang-match :match)))
    ;; we only export if the destination matches the lang set by the export block
    (when (string= (cltpt/base:text-format-name backend)
                   lang)
      (list :text (cltpt/base:text-object-contents obj)
            :recurse nil
            :reparse nil
            :escape nil))))

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